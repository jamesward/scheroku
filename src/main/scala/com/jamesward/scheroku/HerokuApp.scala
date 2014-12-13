package com.jamesward.scheroku

import com.micronautics.scheroku.DynoSizeEnum
import com.micronautics.scheroku.DynoSizeEnum._
import play.api.http.Status
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.ws.WSResponse
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

object HerokuApp {
  def create(maybeName: Option[String])(implicit apiKey: HerokuApiKey, ec: ExecutionContext): Future[HerokuApp] = {
    val body: JsValue = maybeName.map { name => Json.toJson(Map("name" -> name)) }.getOrElse(Json.toJson(Map.empty[String, String]))
    ws("apps").post(body)
      .flatMap(handle(Status.CREATED, _.as[HerokuApp]))
  }

  def get(appName: HerokuAppName)(implicit apiKey: HerokuApiKey, ec: ExecutionContext): Future[Option[HerokuApp]] =
    ws("apps/$appName").get()
      .flatMap { handle(Status.OK, _.as[Seq[HerokuApp]]) } map { _.headOption }

  def getAll(implicit apiKey: HerokuApiKey, ec: ExecutionContext): Future[List[HerokuApp]] =
    ws("apps").get()
      .flatMap(handle(Status.OK, _.as[List[HerokuApp]]))
}

case class HerokuApp(
      name: HerokuAppName,
      web_url: String/*,
      archived_at: Option[DateTime] = None,
      build_stackId: Option[String] = None,
      build_stackName: Option[String] = None,
      created_at: Option[DateTime] = None,
      git_url: Option[String] = None,
      id: Option[String] = None,
      maintenance: Boolean = false,
      ownerEmail: Option[String] = None,
      ownerId: Option[String] = None,
      regionId: Option[String] = None,
      regionName: Option[String] = None,
      released_at: Option[DateTime] = None,
      repo_size: Long = 0L,
      slug_size: Long = 0L,
      stackId: Option[String] = None,
      stackName: String = "cedar",
      updated_at: Option[DateTime] = None, */
    )(implicit val ec: ExecutionContext) extends HerokuAPI {
  implicit val appName: HerokuAppName = name

  /** Appends given configVars to Heroku app's pre-existing config vars
    * @return config vars added, NOT all currently defined config vars */
  def addConfigVars(configVars: ConfigVars)(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[ConfigVars] = {
    ws(s"apps/$appName/config-vars")
      .patch(Json.toJson[ConfigVarMap](configVars.vars))
      .flatMap { handle(Status.OK, jsonToConfigVars(_)(apiKey, appName)) }
  }

  /** @return `Future[ConfigVars]` where `ConfigVars` contains an empty `Map[String, String]` */
  def clearConfigVars()(implicit apiKey: HerokuApiKey): Future[ConfigVars] = {
    configVars.flatMap { cvs =>
      val jsonNulls: Map[String, JsValue] = cvs.vars.mapValues { x => JsNull }
      val jsonClear: JsValue = Json.toJson(jsonNulls)
      ws(s"apps/$name/config-vars").patch(jsonClear)
        .map{ _ => ConfigVars(Map.empty[String, String]) }
    }
  }

  /** @return config vars for this Heroku app */
  def configVars(implicit apiKey: HerokuApiKey): Future[ConfigVars] = {
    ws(s"apps/$name/config-vars").get()
      .flatMap { handle(Status.OK, jsonToConfigVars(_)(apiKey, name)) }
  }

  /** Replaces Heroku app's pre-existing config vars with given configVars */
  def configVars_=(newConfigVars: ConfigVars)(implicit apiKey: HerokuApiKey): Future[ConfigVars] =
    setConfigVars(newConfigVars)(apiKey)

  def setConfigVars(newConfigVars: ConfigVars)(implicit apiKey: HerokuApiKey): Future[ConfigVars] =
    clearConfigVars.flatMap { _ => addConfigVars(newConfigVars)(apiKey, name) }

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
    ws(s"apps/$name/dynos").post(requestJson)
  }

  def destroyApp()(implicit apiKey: HerokuApiKey): Future[WSResponse] =
    ws(s"apps/$name").delete()
}
