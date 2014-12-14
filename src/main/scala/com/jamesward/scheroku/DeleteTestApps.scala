package com.jamesward.scheroku

import HerokuClient._
import play.api.libs.ws.WSResponse
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global

/** Deletes all Heroku apps left over from testing */
object DeleteTestApps extends App {
  val prefix: String = if (args.nonEmpty) args(0) else "test"

  withLogin { implicit herokuApiKey =>
    HerokuApp.getAll.onSuccess { case herokuApps =>
      val deletions: List[Future[WSResponse]] = for {
        herokuApp <- herokuApps if herokuApp.name.appName.startsWith(prefix)
      } yield {
        println(s"Deleting ${herokuApp.name}")
        herokuApp.destroyApp()
      }
      Future.sequence(deletions).onComplete {
        case _ => System.exit(0)
      }
    }
  }
}
