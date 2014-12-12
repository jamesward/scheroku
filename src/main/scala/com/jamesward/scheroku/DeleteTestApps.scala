package com.jamesward.scheroku

import java.io.File
import play.api.libs.ws.WSResponse
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global

/** Deletes all Heroku apps left over from testing */
object DeleteTestApps extends App {
  val prefix: String = if (args.nonEmpty) args(0) else "test"
  val herokuApiUrlStr = "api.heroku.com"

  withLogin(new File(".")) { apiKey =>
    implicit val ak = HerokuApiKey(apiKey)
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

  def withLogin(appDir: File)(fn: String => Unit) = {
    import imagej.updater.webdav.NetrcParser.Credentials

    def withCredentials(credentials: Credentials) {
      import util.{Failure, Success}
      getApiKey(credentials.getUsername, credentials.getPassword).onComplete {
        case Failure(exp) =>
          sys.error(exp.getMessage)

        case Success(value) =>
          fn(value)
      }
    }

    val netRC = new File(System.getProperty("user.home"), ".netrc")
    if (netRC.exists) {
      import imagej.updater.webdav.NetrcParser
      withCredentials(new NetrcParser().getCredentials(herokuApiUrlStr))
    } else {
      (for {
        username <- sys.env.get("HEROKU_USERNAME")
        password <- sys.env.get("HEROKU_PASSWORD")
      } yield {
        import imagej.updater.webdav.NetrcParser.Credentials
        withCredentials(new Credentials(herokuApiUrlStr, username, password))
      }).orElse(sys.error("~/.netrc was not found and environment variables not set, so cannot authenticate with Heroku"))
    }
    ()
  }
}
