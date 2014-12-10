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

object HerokuAPI {
  class HerokuWS extends WSAPI {
    val builder = new NingAsyncHttpClientConfigBuilder(DefaultWSClientConfig())
    override def client: WSClient = new NingWSClient(builder.build())
    override def url(url: String): WSRequestHolder = client.url(url)
  }

  /** Invokes version 1 of the Heroku API for the login API. */
  def wsV1(path: String): WSRequestHolder = new HerokuWS().url(s"https://api.heroku.com/$path")

  /** This method enhances `wsV1` to use any version of the Heroku API. By default, it uses version 3. */
  def ws(path: String, version: String="3")(implicit apiKey: HerokuApiKey): WSRequestHolder =
    wsV1(path)
      .withAuth("", apiKey.toString, WSAuthScheme.BASIC)
      .withHeaders("Accept" -> s"application/vnd.heroku+json; version=$version")

  def getError(response: WSResponse): String =
    (response.json \ "error").asOpt[String]
      .orElse((response.json \ "message").asOpt[String])
      .getOrElse("Unknown Error")

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

  def getApiKey(username: String, password: String)(implicit ec: ExecutionContext): Future[String] =
    wsV1("account")
      .withAuth(username, password, WSAuthScheme.BASIC).get()
      .flatMap(handle(Status.OK, _.\("api_key").as[String]))
}

trait HerokuAPI extends HerokuApiImplicits {
  import HerokuAPI._

  implicit val ec: ExecutionContext

  def createApp()(implicit apiKey: HerokuApiKey): Future[HerokuApp] =
    ws("apps")
      .post(Results.EmptyContent())
      .flatMap(handle(Status.CREATED, _.as[HerokuApp]))

  def appSetup(blobUrl: String)(implicit apiKey: HerokuApiKey): Future[JsValue] = {
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

  def appSetupStatus(id: String)(implicit apiKey: HerokuApiKey): Future[JsValue] =
    ws(s"app-setups/$id", "edge").get()
      .flatMap(handle(Status.OK, identity))

  def destroyApp()(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[_] =
    ws(s"apps/$appName").delete()

  // todo figure out return type for JSON
  def dynoCreate(blobUrl: String)(implicit apiKey: HerokuApiKey): Future[_] = {
    val requestJson = Json.obj("source_blob" -> Json.obj("url" -> blobUrl))
    for {response <- ws("app-setups", "edge").post(requestJson)} yield {
      (response.json \ "id").as[String]
    }
  }

  def dynoRestart(dynoIdOrName: String)(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[_] =
    ws(s"apps/$appName/dynos/$dynoIdOrName").delete()

  // todo figure out return type for JSON
  def dynoInfo(dynoIdOrName: String)(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[_] =
    ws(s"apps/$appName/dynos/$dynoIdOrName").get()

  // todo figure out return type for JSON
  def dynosList(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[_] =
    ws(s"apps/$appName").get()

  def dynosRestartAll(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[_] =
    ws(s"apps/$appName/dynos").delete()

  def getApps(implicit apiKey: HerokuApiKey): Future[Seq[HerokuApp]] =
    ws("apps").get()
      .flatMap(handle(Status.OK, _.as[Seq[HerokuApp]]))

  def logs(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[JsValue] = {
    val requestJson = Json.obj("tail" -> true, "lines" -> 10)
    ws(s"apps/$appName/log-sessions").post(requestJson).flatMap(handle(Status.CREATED, identity))
  }

  def buildResult(id: String)(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[JsValue] =
    ws(s"apps/$appName/builds/$id/result").get()
      .flatMap(handle(Status.OK, identity))

  // todo: logplex doesn't chunk the response so it doesn't show up right away
  def logStream(url: String): Future[(WSResponseHeaders, Enumerator[Array[Byte]])] =
    new HerokuWS().url(url).stream()

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
}

case class HerokuApp(name: HerokuAppName, web_url: String)(implicit val ec: ExecutionContext) extends HerokuAPI {
  import HerokuAPI._

  def buildSlug(url: String)(implicit apiKey: HerokuApiKey): Future[JsValue] = {
    val requestJson = Json.obj("source_blob" -> Json.obj("url" -> url))

    // set the api version to 'edge' in order to get the output_stream_url
    ws(s"apps/$name/builds", "edge").post(requestJson).flatMap(handle(Status.CREATED, identity))
  }

  def createSlug(appDir: File)(implicit apiKey: HerokuApiKey): Future[String] = {
    val requestJson = Json.obj("process_types" -> Json.obj())

    ws(s"/apps/$name/slugs").post(requestJson).flatMap(handleAsync(Status.CREATED, { response =>
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
        ws(s"apps/$name/slugs/$id").get().flatMap(handle(Status.OK, _.\("blob").\("url").as[String]))
      }
    }))
  }

  def getConfigVars(implicit apiKey: HerokuApiKey): Future[JsValue] =
    ws(s"apps/$name/config-vars", apiKey.toString).get().flatMap(handle(Status.OK, identity))

  def setConfigVars(configVars: JsValue)(implicit apiKey: HerokuApiKey): Future[JsValue] =
    ws(s"apps/$name/config-vars", apiKey.toString).patch(configVars).flatMap(handle(Status.OK, identity))
}

object HerokuApp {
  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  import scala.language.implicitConversions

  implicit val herokuAppWrites = new Writes[HerokuApp] {
    def writes(herokuApp: HerokuApp): JsValue =
      Json.obj(
        "name" -> herokuApp.name.toString,
        "web_url" -> herokuApp.web_url
      )
  }

  implicit def herokuAppReads(implicit ec: ExecutionContext): Reads[HerokuApp] = (
      (__ \ "name").read[String].map { name => HerokuAppName(name) } ~
      (__ \ "web_url").read[String]
    )(HerokuApp.apply _)

  implicit val herokuAppNameFormat = Json.format[HerokuAppName]
}

class Dyno {
  // TODO write me
}

/** Provide strong typing for API parameters */
case class HerokuApiKey(apiKey: String) extends AnyVal {
  override def toString = apiKey
}

/** Provide strong typing for API parameters */
case class HerokuAppName(appName: String) extends AnyVal {
  override def toString = appName
}

object HerokuAppName {
  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  implicit val herokuAppNameFormat = Json.format[HerokuAppName]
}

/** Lots of API method parameters are repetitive, so use implicits to clean up API */
trait HerokuApiImplicits {
  implicit class RichString(string: String) {
    def asApiKey = HerokuApiKey(string)

    def asAppName = HerokuAppName(string)
  }
}
