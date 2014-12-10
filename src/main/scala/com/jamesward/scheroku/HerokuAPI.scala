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

  /** Create a new app setup from a gzipped tar archive containing an app.json manifest file */
  // TODO change return type to Future[HerokuApp]
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

  def destroyApp()(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[WSResponse] =
    ws(s"apps/$appName").delete()

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

object HerokuApp {
  import HerokuAPI._
  import play.api.libs.json._
  import play.api.libs.functional.syntax._
  import scala.language.implicitConversions
  import HerokuAppName._

  implicit val herokuAppWrites = new Writes[HerokuApp] {
    def writes(herokuApp: HerokuApp): JsValue =
      Json.obj(
        "name" -> herokuApp.appName.toString,
        "web_url" -> herokuApp.web_url
      )
  }

  implicit def herokuAppReads(implicit ec: ExecutionContext): Reads[HerokuApp] = (
      (__ \ "name").read[String].map { name => HerokuAppName(name) } ~
      (__ \ "web_url").read[String]
    )(HerokuApp.apply _)

  def create()(implicit apiKey: HerokuApiKey, ec: ExecutionContext): Future[HerokuApp] =
    ws("apps")
      .post(Results.EmptyContent())
      .flatMap(handle(Status.CREATED, _.as[HerokuApp]))

  def getAll(implicit apiKey: HerokuApiKey, ec: ExecutionContext): Future[Seq[HerokuApp]] =
    ws("apps").get()
      .flatMap(handle(Status.OK, _.as[Seq[HerokuApp]]))
}

case class HerokuApp(appName: HerokuAppName, web_url: String)(implicit val ec: ExecutionContext) extends HerokuAPI {
  import HerokuAPI._
  import com.micronautics.scheroku.DynoSizeEnum
  import com.micronautics.scheroku.DynoSizeEnum._

  def buildSlug(url: String)(implicit apiKey: HerokuApiKey): Future[JsValue] = {
    val requestJson = Json.obj("source_blob" -> Json.obj("url" -> url))

    // set the api version to 'edge' in order to get the output_stream_url
    ws(s"apps/$appName/builds", "edge").post(requestJson).flatMap(handle(Status.CREATED, identity))
  }

  def createSlug(appDir: File)(implicit apiKey: HerokuApiKey): Future[String] = {
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

  def destroyApp()(implicit apiKey: HerokuApiKey): Future[WSResponse] =
    ws(s"apps/$appName").delete()

  /** @return Typical response (from Heroku docs): <pre>{
    "attach_url": "rendezvous://rendezvous.runtime.heroku.com:5000/{rendezvous-id}",
    "command": "bash",
    "created_at": "2012-01-01T12:00:00Z",
    "id": "01234567-89ab-cdef-0123-456789abcdef",
    "name": "run.1",
    "release": {
      "id": "01234567-89ab-cdef-0123-456789abcdef",
      "version": 11
    },
    "size": "1X",
    "state": "up",
    "type": "run",
    "updated_at": "2012-01-01T12:00:00Z"
  }</pre> */
  def createDyno(command: String, dynoSize: DynoSizeEnum=X1, attach: Option[Boolean]=None, env: Option[Map[String, String]]=None)
                (implicit apiKey: HerokuApiKey): Future[WSResponse] = {
    import collection.mutable

    val params = mutable.Map("command" -> command, "size" -> dynoSize.size)
    attach foreach { a =>  params += "attach" -> a.toString }
    env foreach { e => params += "env" -> Json.toJson(e).toString }
    val requestJson = Json.toJson(params.toMap)
    ws(s"apps/$appName/dynos").post(requestJson)
  }

  def configVars(implicit apiKey: HerokuApiKey): Future[JsValue] =
    ws(s"apps/$appName/config-vars").get().flatMap(handle(Status.OK, identity))

  /** Appends given configVars to Heroku app's pre-existing config vars */
  // TODO redefine as += instead of setter
  def configVars_=(configVars: Map[String, String])(implicit apiKey: HerokuApiKey): Future[JsValue] =
    ws(s"apps/$appName/config-vars").patch(Json.toJson(configVars)).flatMap(handle(Status.OK, identity)) // "edge" did not help

  /** Replaces Heroku app's pre-existing config vars with given configVars */
  def configVars_=(configVars: Map[String, String])(implicit apiKey: HerokuApiKey): Future[JsValue] =
    for {
      reponse1 <- ws(s"apps/$appName/config-vars").patch(Json.toJson(()))
      reponse2 <- ws(s"apps/$appName/config-vars").patch(Json.toJson(configVars))
    } yield handle(Status.OK, identity)(reponse2)
}

object Dyno {
  import HerokuAPI._

  def dynosList(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[WSResponse] =
    ws(s"apps/$appName").get()

  def dynosRestartAll(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[WSResponse] =
    ws(s"apps/$appName/dynos").delete()
}

case class Dyno(dynoIdOrName: String) {
  import HerokuAPI._

  def restart()(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[WSResponse] =
    ws(s"apps/$appName/dynos/$dynoIdOrName").delete()

  // todo figure out return type for JSON
  def dynoInfo(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[WSResponse] =
    ws(s"apps/$appName/dynos/$dynoIdOrName").get()
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
  implicit class RichWSResponse(wsResponse: WSResponse) {
    import play.api.libs.json._
    import play.api.libs.functional.syntax._

    implicit val dynoInfoFormat = Json.format[DynoInfo]

    def asDynoInfo: DynoInfo = Json.fromJson(wsResponse.json).get
  }

  implicit class RichString(string: String) {
    def asApiKey = HerokuApiKey(string)

    def asAppName = HerokuAppName(string)
  }
}

case class DynoInfo(
  attach_url: String,
  command: String,
  created_at: String,
  id: String,
  name: String,
  release: Map[String, String],
  size: String,
  state: String,
  `type`: String,
  updated_at: String
 )
