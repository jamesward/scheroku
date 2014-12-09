package com.jamesward.scheroku

import java.io.{BufferedOutputStream, File, FileInputStream, FileOutputStream}
import java.util.zip.GZIPOutputStream

import akka.actor.ActorSystem
import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveOutputStream}
import org.apache.commons.compress.utils.IOUtils
import play.api.http.Status
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsValue, Json}
import play.api.libs.ws.{DefaultWSClientConfig, WSAPI, WSAuthScheme, WSClient, WSRequestHolder, WSResponse, WSResponseHeaders}
import play.api.libs.ws.ning.{NingAsyncHttpClientConfigBuilder, NingWSClient}
import play.api.mvc.Results

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}

/** Provide strong typing for API parameters */
case class APIKey(apiKey: String) extends AnyVal {
  override def toString = apiKey
}

/** Provide strong typing for API parameters */
case class AppName(appName: String) extends AnyVal {
  override def toString = appName
}

/** Lots of API method parameters are repetitive, so use implicits to clean up API */
trait HerokuApiImplicits {
  implicit class RichString(string: String) {
    def asApiKey = APIKey(string)

    def asAppName = AppName(string)
  }
}

trait HerokuAPI extends HerokuApiImplicits {
  implicit val ec: ExecutionContext

  class HerokuWS extends WSAPI {
    val builder = new NingAsyncHttpClientConfigBuilder(DefaultWSClientConfig())
    override def client: WSClient = new NingWSClient(builder.build())
    override def url(url: String): WSRequestHolder = client.url(url)
  }

  /** This method is just for the login API, which is only available from the old Heroku API. */
  def wsOld(path: String): WSRequestHolder = new HerokuWS().url(s"https://api.heroku.com/$path")

  // this one uses version 3
  def ws(path: String, version: String = "3")(implicit apiKey: APIKey): WSRequestHolder =
    wsOld(path).withAuth("", apiKey.toString, WSAuthScheme.BASIC).withHeaders("Accept" -> s"application/vnd.heroku+json; version=$version")

  def getError(response: WSResponse): String =
    (response.json \ "error").asOpt[String].orElse((response.json \ "message").asOpt[String]).getOrElse("Unknown Error")

  def handleAsync[A](status: Int, block: JsValue => Future[A])(response: WSResponse): Future[A] = {
    response.status match {
      case s: Int if s == status =>
        block(response.json)

      case _ => // usually an error of some sort
        Future.failed(new RuntimeException(getError(response)))
    }
  }

  def handle[A](status: Int, block: JsValue => A)(response: WSResponse): Future[A] = {
    response.status match {
      case s: Int if s == status =>
        Future.successful(block(response.json))

      case _ => // usually an error of some sort
        Future.failed(new RuntimeException(getError(response)))
    }
  }

  def getApiKey(username: String, password: String): Future[String] =
    wsOld("account").withAuth(username, password, WSAuthScheme.BASIC).get().flatMap(handle(Status.OK, _.\("api_key").as[String]))

  def createApp()(implicit apiKey: APIKey): Future[App] =
    ws("apps").post(Results.EmptyContent()).flatMap(handle(Status.CREATED, _.as[App]))

  def appSetup(blobUrl: String)(implicit apiKey: APIKey): Future[JsValue] = {
    val requestJson = Json.obj("source_blob" -> Json.obj("url" -> blobUrl))
    ws("app-setups", "edge").post(requestJson).flatMap { response =>
      val id = (response.json \ "id").as[String]

      // poll for completion
      val appSetupPromise = Promise[JsValue]()
      val tick = ActorSystem().scheduler.schedule(Duration.Zero, 1.second, new Runnable {
        override def run() = {
          appSetupStatus(id).foreach { json =>
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

  def appSetupStatus(id: String)(implicit apiKey: APIKey): Future[JsValue] =
    ws(s"app-setups/$id",  "edge").get().flatMap(handle(Status.OK, identity))

  def destroyApp()(implicit apiKey: APIKey, appName: AppName): Future[_] =
    ws(s"apps/$appName").delete()

  // todo figure out return type for JSON
  def dynoCreate(blobUrl: String)(implicit apiKey: APIKey): Future[_] = {
    val requestJson = Json.obj("source_blob" -> Json.obj("url" -> blobUrl))
    for { response <- ws("app-setups", "edge").post(requestJson) } yield {
      (response.json \ "id").as[String]
    }
  }

  def dynoRestart(dynoIdOrName: String)(implicit apiKey: APIKey, appName: AppName): Future[_] =
    ws(s"apps/$appName/dynos/$dynoIdOrName").delete()

  // todo figure out return type for JSON
  def dynoInfo(dynoIdOrName: String)(implicit apiKey: APIKey, appName: AppName): Future[_] =
    ws(s"apps/$appName/dynos/$dynoIdOrName").get()

  // todo figure out return type for JSON
  def dynosList(implicit apiKey: APIKey, appName: AppName): Future[_] =
    ws(s"apps/$appName").get()

  def dynosRestartAll(implicit apiKey: APIKey, appName: AppName): Future[_] =
    ws(s"apps/$appName/dynos").delete()

  def getApps(implicit apiKey: APIKey): Future[Seq[App]] =
    ws("apps").get().flatMap(handle(Status.OK, _.as[Seq[App]]))

  def logs(implicit apiKey: APIKey, appName: AppName): Future[JsValue] = {
    val requestJson = Json.obj("tail" -> true, "lines" -> 10)
    ws(s"apps/$appName/log-sessions").post(requestJson).flatMap(handle(Status.CREATED, identity))
  }

  def buildResult(id: String)(implicit apiKey: APIKey, appName: AppName): Future[JsValue] =
    ws(s"apps/$appName/builds/$id/result").get().flatMap(handle(Status.OK, identity))

  // todo: logplex doesn't chunk the response so it doesn't show up right away
  def logStream(url: String): Future[(WSResponseHeaders, Enumerator[Array[Byte]])] =
    new HerokuWS().url(url).stream()

  def createSlug(appDir: File)(implicit apiKey: APIKey, appName: AppName): Future[String] = {
    val requestJson = Json.obj("process_types" -> Json.obj())

    ws(s"/apps/$appName/slugs").post(requestJson).flatMap(handleAsync(Status.CREATED, { response =>
      val id = (response \ "id").as[String]
      val url = (response \ "blob" \ "url").as[String]
      val tgzFile = new File(sys.props("java.io.tmpdir"), System.nanoTime().toString + ".tar.gz")

      val tgzos = new TarArchiveOutputStream(new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(tgzFile))))
      if (appDir.listFiles != null)  // start with the files, not the dir
        appDir.listFiles.foreach { file => addToTar(tgzos, file.getAbsolutePath, "") }
      tgzos.finish()
      tgzos.close()

      // put the tgz
      new HerokuWS().url(url).put(tgzFile).flatMap { _ =>
        tgzFile.delete()

        // get the url to the slug
        ws(s"apps/$appName/slugs/$id").get().flatMap(handle(Status.OK, _.\("blob").\("url").as[String]))
      }
    }))
  }

  /** side effecting!!! */
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

  def buildSlug(url: String)(implicit apiKey: APIKey, appName: AppName): Future[JsValue] = {
    val requestJson = Json.obj("source_blob" -> Json.obj("url" -> url))

    // set the api version to 'edge' in order to get the output_stream_url
    ws(s"apps/$appName/builds", "edge").post(requestJson).flatMap(handle(Status.CREATED, identity))
  }

  def getConfigVars(implicit apiKey: APIKey, appName: AppName): Future[JsValue] =
    ws(s"apps/$appName/config-vars", apiKey.toString).get().flatMap(handle(Status.OK, identity))

  def setConfigVars(configVars: JsValue)(implicit apiKey: APIKey, appName: AppName): Future[JsValue] =
    ws(s"apps/$appName/config-vars", apiKey.toString).patch(configVars).flatMap(handle(Status.OK, identity))

  case class App(name: String, web_url: String)

  implicit val appFormat = Json.format[App]
}
