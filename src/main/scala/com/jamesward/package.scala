package com.jamesward

import play.api.http.Status
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.ws.{WSAuthScheme, WSRequestHolder, WSResponse}
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

package object scheroku extends HerokuApiImplicits {
  type ConfigVarMap = Map[String, Any]
  type StringMap     = Map[String, String]

  def getApiKey(username: String, password: String)(implicit ec: ExecutionContext): Future[String] =
    wsV1("account")
      .withAuth(username, password, WSAuthScheme.BASIC).get()
      .flatMap(handle(Status.OK, _.\("api_key").as[String]))

  def getError(response: WSResponse): String =
    (response.json \ "error").asOpt[String]
      .orElse((response.json \ "message").asOpt[String])
      .getOrElse("Unknown Error")

  def handle[A](status: Int, block: JsValue => A)(response: WSResponse): Future[A] = {
    response.status match {
      case s: Int if s == status =>
        Future.successful(block(response.json))

      case _ => // usually an error of some sort
        Future.failed(new RuntimeException(getError(response)))
    }
  }

  def handleAsync[A](status: Int, block: JsValue => Future[A])(response: WSResponse): Future[A] = {
    response.status match {
      case s: Int if s == status =>
        block(response.json)

      case _ => // usually an error of some sort
        Future.failed(new RuntimeException(getError(response)))
    }
  }

  implicit def herokuAppWrites(implicit ec: ExecutionContext): Writes[HerokuApp] = new Writes[HerokuApp] {
    def writes(herokuApp: HerokuApp): JsValue =
      Json.obj(
        "name"    -> herokuApp.appName.toString,
        "web_url" -> herokuApp.web_url
      )
  }

  implicit def herokuAppReads(implicit ec: ExecutionContext): Reads[HerokuApp] = (
      (__ \ "name"   ).read[String].map { name => HerokuAppName(name) } ~
      (__ \ "web_url").read[String]
    )(HerokuApp.apply _)

  def jsonToConfigVars(jsValue: JsValue)(implicit apiKey: HerokuApiKey, appName: HerokuAppName): ConfigVars =
    ConfigVars(Json.fromJson[ConfigVarMap](jsValue).get)

  /** This method enhances `wsV1` to use any version of the Heroku API. By default, it uses version 3. */
  def ws(path: String, version: String="3")(implicit apiKey: HerokuApiKey): WSRequestHolder =
    wsV1(path)
      .withAuth("", apiKey.toString, WSAuthScheme.BASIC)
      .withHeaders("Accept" -> s"application/vnd.heroku+json; version=$version")

  /** Invokes version 1 of the Heroku API for the login API. */
  def wsV1(path: String): WSRequestHolder = new HerokuWS().url(s"https://api.heroku.com/$path")

  implicit val dynoInfoFormat = Json.format[DynoInfo]

  implicit val herokuAppNameFormat = Json.format[HerokuAppName]
}
