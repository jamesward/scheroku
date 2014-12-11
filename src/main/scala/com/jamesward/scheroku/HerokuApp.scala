package com.jamesward.scheroku

import java.io.File
import com.micronautics.scheroku.DynoSizeEnum
import com.micronautics.scheroku.DynoSizeEnum._
import play.api.http.Status
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.libs.ws.WSResponse
import play.api.mvc.Results
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

object HerokuApp {
  def create()(implicit apiKey: HerokuApiKey, ec: ExecutionContext): Future[HerokuApp] =
    ws("apps").post(Results.EmptyContent())
      .flatMap(handle(Status.CREATED, _.as[HerokuApp]))

  def get(appName: HerokuAppName)(implicit apiKey: HerokuApiKey, ec: ExecutionContext): Future[Option[HerokuApp]] =
    ws("apps/$appName").get()
      .flatMap { handle(Status.OK, _.as[Seq[HerokuApp]]) } map { _.headOption }

  def getAll(implicit apiKey: HerokuApiKey, ec: ExecutionContext): Future[Seq[HerokuApp]] =
    ws("apps").get()
      .flatMap(handle(Status.OK, _.as[Seq[HerokuApp]]))
}

case class HerokuApp(appName: HerokuAppName, web_url: String)(implicit val ec: ExecutionContext) extends HerokuAPI {
  /** Appends given configVars to Heroku app's pre-existing config vars
    * @return config vars added, NOT all currently defined config vars */
  def addConfigVars(configVars: ConfigVars)(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[ConfigVars] = {
    println(s"addConfigVars: ${configVars.vars} for ${appName.appName}")
    ws(s"apps/$appName/config-vars")
      .patch(Json.toJson[ConfigVarMap](configVars.vars))
      .flatMap { handle(Status.OK, jsonToConfigVars(_)(apiKey, appName)) }
  }

  def buildSlug(url: String)(implicit apiKey: HerokuApiKey): Future[JsValue] = {
    val requestJson = Json.obj("source_blob" -> Json.obj("url" -> url))

    // set the api version to 'edge' in order to get the output_stream_url
    ws(s"apps/$appName/builds", "edge").post(requestJson).flatMap(handle(Status.CREATED, identity))
  }

  /** @return `Future[ConfigVars]` where `ConfigVars` contains an empty `Map[String, String]` */
  def clearConfigVars()(implicit apiKey: HerokuApiKey): Future[ConfigVars] = {
    println(s"Clear ConfigVars for ${appName.appName}")
    implicit val an = appName
    val futureJson: Future[Future[WSResponse]] =
      for {
        cvs <- configVars
      } yield {
        val jsonNulls: List[String] = for {
          (name, value) <- cvs.vars.toList
        } yield s""""$name" : null"""
        val jsonClear: String = jsonNulls.mkString("{ ", ", " , " }")
        ws(s"apps/$appName/config-vars").patch(jsonClear)
      }
    futureJson.flatMap(identity).andThen { case _ => ConfigVars(Map.empty[String, String]) }.mapTo[ConfigVars]
  }

  /** @return config vars for this Heroku app */
  def configVars(implicit apiKey: HerokuApiKey): Future[ConfigVars] = {
    println(s"Get configVars from ${appName.appName}")
    val x = ws(s"apps/$appName/config-vars")
      x.get()
      .flatMap { handle(Status.OK, jsonToConfigVars(_)(apiKey, appName)) }
  }

  /** Replaces Heroku app's pre-existing config vars with given configVars */
  def configVars_=(newConfigVars: ConfigVars)(implicit apiKey: HerokuApiKey): Future[ConfigVars] =
    setConfigVars(newConfigVars)(apiKey)

  def setConfigVars(newConfigVars: ConfigVars)(implicit apiKey: HerokuApiKey): Future[ConfigVars] = {
    println(s"replaceConfigVars with ${newConfigVars.vars}")
    clearConfigVars.andThen { case _ => addConfigVars(newConfigVars)(apiKey, appName) }
  }

  /** @param command The dyno terminates if the command returns an error
   * @return Typical response (from Heroku docs): <pre>{
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
  def createDyno(command: String, dynoSize: DynoSizeEnum=X1, env: Option[Map[String, String]]=None, attach: Boolean = true)
                (implicit apiKey: HerokuApiKey): Future[WSResponse] = {
    import scala.collection.mutable

    val params = mutable.Map("command" -> command, "size" -> dynoSize.size)
    if (attach) params += "attach" -> "true"
    env foreach { e => params += "env" -> Json.toJson[StringMap](e).toString }
    val requestJson = Json.toJson[StringMap](params.toMap)
    println(s"createDyno requestJson=$requestJson")
    ws(s"apps/$appName/dynos").post(requestJson)
  }

  def createSlug(appDir: File)(implicit apiKey: HerokuApiKey): Future[String] = {
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

        // get the url to the slug
        ws(s"apps/$appName/slugs/$id").get().flatMap(handle(Status.OK, _.\("blob").\("url").as[String]))
      }
    }))
  }

  def destroyApp()(implicit apiKey: HerokuApiKey): Future[WSResponse] =
    ws(s"apps/$appName").delete()
}
