package com.jamesward.scheroku

import java.io.{FileInputStream, FileOutputStream, BufferedOutputStream, File}
import java.util.zip.GZIPOutputStream

import akka.actor.ActorSystem
import play.api.http.Status
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.ws.DefaultWSClientConfig
import play.api.libs.ws.WSAPI
import play.api.libs.ws.WSAuthScheme
import play.api.libs.ws.WSClient
import play.api.libs.ws.WSRequestHolder
import play.api.libs.ws.WSResponse
import play.api.libs.ws.WSResponseHeaders
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import play.api.libs.ws.ning.NingWSClient
import play.api.mvc.Results
import org.apache.commons.compress.archivers.tar.TarArchiveEntry
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream
import org.apache.commons.compress.utils.IOUtils

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Promise, Future}


trait HerokuAPI {

  implicit val ec: ExecutionContext

  var BASE_URL = "https://api.heroku.com/%s"

  class HerokuWS extends WSAPI {
    val builder = new NingAsyncHttpClientConfigBuilder(DefaultWSClientConfig())
    override def client: WSClient = new NingWSClient(builder.build())
    override def url(url: String): WSRequestHolder = client.url(url)
  }

  // this uses an older API version by default because the login API we are using is only in the old API
  def ws(path: String): WSRequestHolder = new HerokuWS().url(BASE_URL.format(path))

  // this one uses version 3
  def ws(path: String, apiKey: String, version: String = "3"): WSRequestHolder =
    ws(path).withAuth("", apiKey, WSAuthScheme.BASIC).withHeaders("Accept" -> s"application/vnd.heroku+json; version=$version")

  def getError(response: WSResponse): String =
    (response.json \ "error").asOpt[String].orElse((response.json \ "message").asOpt[String]).getOrElse("Unknown Error")

  def handleAsync[A](status: Int, block: JsValue => Future[A])(response: WSResponse): Future[A] = {
    response.status match {
      case s: Int if s == status =>
        block(response.json)
      case _ =>
        // usually an error of some sort
        Future.failed(new RuntimeException(getError(response)))
    }
  }

  def handle[A](status: Int, block: JsValue => A)(response: WSResponse): Future[A] = {
    response.status match {
      case s: Int if s == status =>
        Future.successful(block(response.json))
      case _ =>
        // usually an error of some sort
        Future.failed(new RuntimeException(getError(response)))
    }
  }

  def getApiKey(username: String, password: String): Future[String] = {
    ws("account").withAuth(username, password, WSAuthScheme.BASIC).get().flatMap(handle(Status.OK, _.\("api_key").as[String]))
  }

  def createApp(apiKey: String): Future[App] = {
    ws("apps", apiKey).post(Results.EmptyContent()).flatMap(handle(Status.CREATED, _.as[App]))
  }

  def appSetup(apiKey: String, blobUrl: String): Future[JsValue] = {
    val requestJson = Json.obj("source_blob" -> Json.obj("url" -> blobUrl))
    ws("app-setups", apiKey, "edge").post(requestJson).flatMap { response =>
      val id = (response.json \ "id").as[String]

      // poll for completion

      val appSetupPromise = Promise[JsValue]()

      val tick = ActorSystem().scheduler.schedule(Duration.Zero, 1.second, new Runnable {
        override def run() = {
          appSetupStatus(apiKey, id).foreach { json =>
            val status = (json \ "status").as[String]
            status match {
              case "failed" =>
                val message = (json \ "failure_message").as[String] + " " + (json \ "manifest_errors").as[Seq[String]].mkString
                appSetupPromise.tryFailure(new RuntimeException(message))
              case "succeeded" =>
                appSetupPromise.trySuccess(json)
              case "pending" =>
                // see if the build has started
                // once the build starts we complete the promise
                if ((json \ "build" \ "id").asOpt[String].isDefined) {
                  appSetupPromise.trySuccess(json)
                }
            }
          }
        }
      })

      appSetupPromise.future.onComplete(_ => tick.cancel())

      appSetupPromise.future
    }
  }

  def appSetupStatus(apiKey: String, id: String): Future[JsValue] = {
    ws(s"app-setups/$id", apiKey, "edge").get().flatMap(handle(Status.OK, identity))
  }

  def destroyApp(apiKey: String, appName: String): Future[_] = {
    ws(s"apps/$appName", apiKey).delete()
  }

  def getApps(apiKey: String): Future[Seq[App]] = {
    ws("apps", apiKey).get().flatMap(handle(Status.OK, _.as[Seq[App]]))
  }

  def logs(apiKey: String, appName: String): Future[JsValue] = {
    val requestJson = Json.obj("tail" -> true, "lines" -> 10)
    ws(s"apps/$appName/log-sessions", apiKey).post(requestJson).flatMap(handle(Status.CREATED, identity))
  }

  def buildResult(apiKey: String, appName: String, id: String): Future[JsValue] = {
    ws(s"apps/$appName/builds/$id/result", apiKey).get().flatMap(handle(Status.OK, identity))
  }

  // todo: logplex doesn't chunk the response so it doesn't show up right away
  def logStream(url: String): Future[(WSResponseHeaders, Enumerator[Array[Byte]])] = {
    new HerokuWS().url(url).stream()
  }

  def createSlug(apiKey: String, appName: String, appDir: File): Future[String] = {
    val requestJson = Json.obj("process_types" -> Json.obj())

    ws(s"/apps/$appName/slugs", apiKey).post(requestJson).flatMap(handleAsync(Status.CREATED, { response =>
      val id = (response \ "id").as[String]

      val url = (response \ "blob" \ "url").as[String]

      val tgzFile = new File(sys.props("java.io.tmpdir"), System.nanoTime().toString + ".tar.gz")

      // create the tgz
      val tgzos = new TarArchiveOutputStream(new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(tgzFile))))

      // start with the files, not the dir
      if (appDir.listFiles != null) {
        appDir.listFiles.foreach { file =>
          addToTar(tgzos, file.getAbsolutePath, "")
        }
      }

      tgzos.finish()

      tgzos.close()

      // put the tgz
      new HerokuWS().url(url).put(tgzFile).flatMap { _ =>
        tgzFile.delete()

        // get the url to the slug
        ws(s"apps/$appName/slugs/$id", apiKey).get().flatMap(handle(Status.OK, _.\("blob").\("url").as[String]))
      }
    }))
  }

  // side effecting!!!
  def addToTar(tOut: TarArchiveOutputStream, path: String, base: String): Unit = {
    // manual exclude of target dirs & local.conf
    if (!base.endsWith("target/") && !path.endsWith("target") && !path.endsWith("local.conf")) {
      val f = new File(path)
      val entryName = base + f.getName
      val tarEntry = new TarArchiveEntry(f, entryName)
      tOut.putArchiveEntry(tarEntry)

      if (f.isFile) {
        IOUtils.copy(new FileInputStream(f), tOut)
        tOut.closeArchiveEntry()
      } else {
        tOut.closeArchiveEntry()
        f.listFiles.foreach { child =>
          addToTar(tOut, child.getAbsolutePath, entryName + "/")
        }
      }
    }
  }

  def buildSlug(apiKey: String, appName: String, url: String): Future[JsValue] = {
    val requestJson = Json.obj("source_blob" -> Json.obj("url" -> url))

    // set the api version to 'edge' in order to get the output_stream_url
    ws(s"apps/$appName/builds", apiKey, "edge").post(requestJson).flatMap(handle(Status.CREATED, identity))
  }

  def getConfigVars(apiKey: String, appName: String): Future[JsValue] = {
    ws(s"apps/$appName/config-vars", apiKey).get().flatMap(handle(Status.OK, identity))
  }

  def setConfigVars(apiKey: String, appName: String, configVars: JsValue): Future[JsValue] = {
    ws(s"apps/$appName/config-vars", apiKey).patch(configVars).flatMap(handle(Status.OK, identity))
  }

  case class App(name: String, web_url: String)

  implicit val appFormat = Json.format[App]

}