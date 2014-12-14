package com.jamesward.scheroku

import java.io.File
import imagej.updater.webdav.NetrcParser.Credentials
import scala.concurrent.{ExecutionContext, Future}

object HerokuClient {
  val herokuApiUrlStr = "api.heroku.com"

  def withLogin(fn: HerokuApiKey => Unit)(implicit ec: ExecutionContext): Future[Unit] = {
    import scala.concurrent.Future

    def evaluateIfLoggedIn(credentials: Credentials): Future[Unit] = {
      getApiKey(credentials.getUsername, credentials.getPassword).map { apiKey: String =>
        fn(apiKey.asApiKey)
      }
    }

    val netRC = new File(System.getProperty("user.home"), ".netrc")
    if (netRC.exists) {
      import imagej.updater.webdav.NetrcParser
      evaluateIfLoggedIn(new NetrcParser().getCredentials(herokuApiUrlStr))
    } else {
      (for {
         username <- sys.env.get("HEROKU_USERNAME")
         password <- sys.env.get("HEROKU_PASSWORD")
       } yield evaluateIfLoggedIn(new Credentials(herokuApiUrlStr, username, password)))
      .getOrElse(Future.failed(new Exception("~/.netrc was not found and environment variables not set, so cannot authenticate with Heroku")))
    }
  }
}
