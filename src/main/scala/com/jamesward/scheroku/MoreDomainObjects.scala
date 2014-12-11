package com.jamesward.scheroku

import play.api.libs.json._
import play.api.libs.ws.ning.{NingWSRequestHolder, NingAsyncHttpClientConfigBuilder, NingWSClient}
import play.api.libs.ws.{DefaultWSClientConfig, WSAPI, WSClient, WSRequestHolder, WSResponse}
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
