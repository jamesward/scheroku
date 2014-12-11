package com.jamesward.scheroku

import java.net.URL
import org.joda.time.DateTime
import play.api.http.Status
import play.api.libs.json._
import play.api.libs.ws.ning.{ NingAsyncHttpClientConfigBuilder, NingWSClient }
import play.api.libs.ws.{ DefaultWSClientConfig, WSAPI, WSClient, WSRequestHolder, WSResponse }
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

case class ConfigVars(vars: ConfigVarMap)(implicit apiKey: HerokuApiKey, appName: HerokuAppName) {
  /** @return Future[ConfigVars] with given configVars appended to the Heroku app's pre-existing config vars */
//  def +=(configVars: ConfigVarType)(implicit ec: ExecutionContext): Future[ConfigVars] =
//    ws(s"apps/$appName/config-vars")
//      .patch(Json.toJson(configVars))
//      .flatMap(handle(Status.OK, jsonToConfigVars(_)(apiKey, appName)))
}

object Dyno {
  def dynosList(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[WSResponse] =
    ws(s"apps/$appName").get()

  def dynosRestartAll(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[WSResponse] =
    ws(s"apps/$appName/dynos").delete()
}

case class Dyno(dynoIdOrName: String) {
  def restart()(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[WSResponse] =
    ws(s"apps/$appName/dynos/$dynoIdOrName").delete()

  // todo figure out return type for JSON
  def dynoInfo(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[WSResponse] =
    ws(s"apps/$appName/dynos/$dynoIdOrName").get()
}

object Slug {
  import java.io.File
  import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream
  import org.joda.time.format.ISODateTimeFormat

  val isoDateTimeparser = ISODateTimeFormat.dateTimeNoMillis

  /** Side effecting */
  def addToTar(tOut: TarArchiveOutputStream, path: String, base: String): Unit = {
    // manual exclude of target dirs & local.conf
    if (!base.endsWith("target/") && !path.endsWith("target") && !path.endsWith("local.conf")) {
      import org.apache.commons.compress.archivers.tar.TarArchiveEntry
      val f = new File(path)
      val entryName = base + f.getName
      val tarEntry = new TarArchiveEntry(f, entryName)
      tOut.putArchiveEntry(tarEntry)

      if (f.isFile) {
        import java.io.FileInputStream

        import org.apache.commons.compress.utils.IOUtils
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

  /** This is the preferred method for creating slugs */
  def build(url: String)(implicit apiKey: HerokuApiKey, appName: HerokuAppName, ec: ExecutionContext): Future[Slug] = {
    val requestJson = Json.obj("source_blob" -> Json.obj("url" -> url))

    // set the api version to 'edge' in order to get the output_stream_url
    /* jsValue looks like: {
      "created_at": "2012-01-01T12:00:00Z",
      "id": "01234567-89ab-cdef-0123-456789abcdef",
      "source_blob": {
        "url": "https://example.com/source.tgz?token=xyz",
        "version": "v1.3.0"
      },
      "slug": {
        "id": "01234567-89ab-cdef-0123-456789abcdef"
      },
      "status": "succeeded",
      "updated_at": "2012-01-01T12:00:00Z",
      "user": {
        "id": "01234567-89ab-cdef-0123-456789abcdef",
        "email": "username@example.com"
      }
    } */
    for {
      jsValue <- ws(s"apps/$appName/builds", "edge").post(requestJson).flatMap(handle(Status.CREATED, identity))
      status = Json.fromJson[String](jsValue \ "status").get if status=="succeeded" || status=="pending"
    } yield {
      val created = isoDateTimeparser.parseDateTime(Json.fromJson[String](jsValue \ "created_at").get)
      val updated = isoDateTimeparser.parseDateTime(Json.fromJson[String](jsValue \ "updated_at").get)
      Slug(
        created,
        Json.fromJson[String](jsValue \ "id").get,
        new URL(Json.fromJson[String](jsValue \ "source_blob" \ "url").get),
        Json.fromJson[String](jsValue \ "source_blob" \ "version").asOpt,
        updated,
        Json.fromJson[String](jsValue \ "user" \ "id").asOpt,
        Json.fromJson[String](jsValue \ "user" \ "email").asOpt
      )
    }
  }

  /** From the [docs](https://devcenter.heroku.com/articles/platform-api-deploying-slugs?preview=1)
    * "For most uses, however, we recommend using the build resource of the Platform API to transform source code into slugs.
    * Using the build resource is generally simpler than creating slugs from scratch, and will cause slugs to be generated
    * using the standard Heroku slug compiler and the standard Heroku buildspacks. Slugs generated using the standard build
    * pipeline are more likely to be compatible and work on Heroku. For details on how to create slugs using the Platform API
    * and the build resource, see Build and release using the API."
    * @return the url for the new slug */
  def create(appDir: File)(implicit apiKey: HerokuApiKey, appName: HerokuAppName, ec: ExecutionContext): Future[String] = {
    import play.api.http.Status
    val requestJson = Json.obj("process_types" -> Json.obj())

    ws(s"/apps/$appName/slugs").post(requestJson).flatMap(handleAsync(Status.CREATED, { response =>
      import java.io.{BufferedOutputStream, FileOutputStream}
      import java.util.zip.GZIPOutputStream
      import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream

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
        ws(s"apps/$appName/slugs/$id").get().flatMap(handle(Status.OK, _.\("blob").\("url").as[String]))
      }
    }))
  }
}

case class Slug(created: DateTime, id: String, sourceUrl: URL, maybeVersion: Option[String], updated: DateTime, maybeUserId: Option[String], maybeUserEmail: Option[String])

/** Lots of API method parameters are repetitive, so use implicits to clean up API */
trait HerokuApiImplicits {
  implicit class RichWSResponse(wsResponse: WSResponse) {
    def asDynoInfo: DynoInfo = Json.fromJson[DynoInfo](wsResponse.json).get
  }

  implicit class RichString(string: String) {
    def asApiKey = HerokuApiKey(string)

    def asAppName = HerokuAppName(string)
  }

  implicit def stringToAnyFormat: Format[Map[String, Any]] = new Format[Map[String, Any]] {
    def writes(strToT: Map[String, Any]): JsValue =
      Json.parse(scala.util.parsing.json.JSONObject(strToT).toString())

    def reads(jsValue: JsValue): JsResult[Map[String, Any]] =
      JsSuccess(jsValue.as[Map[String, JsValue]].map { case (key, value) =>
        key -> (value match {
          case string: JsString => string.as[String]
          case list => list.as[List[String]]
        })
      })
  }
}

class HerokuWS extends WSAPI {
  val builder = new NingAsyncHttpClientConfigBuilder(DefaultWSClientConfig())

  override def client: WSClient = new NingWSClient(builder.build())

  override def url(url: String): WSRequestHolder = client.url(url)
}
