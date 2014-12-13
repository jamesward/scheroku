package com.jamesward.scheroku

import akka.actor.ActorSystem
import play.api.http.Status
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsValue, Json}
import play.api.libs.ws.{WSResponse, WSResponseHeaders}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}

trait HerokuAPI extends HerokuApiImplicits {
  /** Create a new app setup from a gzipped tar archive containing an app.json manifest file
    * @return {
      "id": "01234567-89ab-cdef-0123-456789abcdef",
      "created_at": "2012-01-01T12:00:00Z",
      "updated_at": "2012-01-01T12:00:00Z",
      "status": "succeeded",
      "failure_message": "invalid app.json",
      "app": {
        "id": "01234567-89ab-cdef-0123-456789abcdef",
        "name": "example"
      },
      "build": {
        "id": "01234567-89ab-cdef-0123-456789abcdef",
        "status": "succeeded"
      },
      "manifest_errors": [
        "config var FOO is required"
      ],
      "postdeploy": {
        "output": "assets precompiled",
        "exit_code": 1
      },
      "resolved_success_url": "http://example.herokuapp.com/welcome"
    } */
  // TODO change return type to Future[HerokuApp]
  def appSetup(blobUrl: String)(implicit apiKey: HerokuApiKey, ec: ExecutionContext): Future[JsValue] = {
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

  def appSetupStatus(id: String)(implicit apiKey: HerokuApiKey, ec: ExecutionContext): Future[JsValue] =
    ws(s"app-setups/$id", "edge").get()
      .flatMap(handle(Status.OK, identity))

  /** Won't work if the app has dynos */
  def destroyApp()(implicit apiKey: HerokuApiKey, appName: HerokuAppName): Future[WSResponse] =
    ws(s"apps/$appName").delete()

  def logs(implicit apiKey: HerokuApiKey, appName: HerokuAppName, ec: ExecutionContext): Future[JsValue] = {
    val requestJson = Json.obj("tail" -> true, "lines" -> 10)
    ws(s"apps/$appName/log-sessions").post(requestJson).flatMap(handle(Status.CREATED, identity))
  }

  def buildResult(id: String)(implicit apiKey: HerokuApiKey, appName: HerokuAppName, ec: ExecutionContext): Future[JsValue] =
    ws(s"apps/$appName/builds/$id/result").get()
      .flatMap(handle(Status.OK, identity))

  // todo: logplex doesn't chunk the response so it doesn't show up right away
  def logStream(url: String): Future[(WSResponseHeaders, Enumerator[Array[Byte]])] =
    new HerokuWS().url(url).stream()
}
