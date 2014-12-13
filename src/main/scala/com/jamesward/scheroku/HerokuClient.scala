package com.jamesward.scheroku

import java.io.File
import scala.concurrent.ExecutionContext

object HerokuClient {
  val herokuApiUrlStr = "api.heroku.com"

  def withLogin(appDir: File)(fn: String => Unit)(implicit ec: ExecutionContext) = {
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
