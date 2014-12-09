package com.jamesward.scheroku

import java.io.File

import org.scalatest.time.{Seconds, Span}
import org.scalatest.{WordSpec, MustMatchers}
import org.scalatest.concurrent.ScalaFutures

import play.api.libs.json.Json

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}

import scala.concurrent.ExecutionContext.Implicits.global

object TestHerokuAPI extends HerokuAPI {
  override val ec = global
}

class HerokuAPISpec extends WordSpec with MustMatchers with ScalaFutures with HerokuApiImplicits {
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(Span(30, Seconds))
  "login with invalid credentials" must {
    "fail" in {
      TestHerokuAPI.getApiKey("foo@foo.com", "bar").onSuccess { case _ => fail("should fail") }
    }
  }

  def withLogin(appDir: File)(testCode: Option[String] => Any) = {
    val netRC = new File(System.getProperty("user.home"), ".netrc")
    val maybeAuthKey: Option[String] = if (netRC.exists) {
      import imagej.updater.webdav.NetrcParser
      import imagej.updater.webdav.NetrcParser.Credentials

      val credentials: Credentials = new NetrcParser().getCredentials("api.heroku.com")
      val f = TestHerokuAPI.getApiKey(credentials.getUsername, credentials.getPassword)
      f.onFailure { case exp =>
        fail(exp)
      }
      Some(f.futureValue)
    } else {
      assume(sys.env.get("HEROKU_USERNAME").isDefined)
      assume(sys.env.get("HEROKU_PASSWORD").isDefined)
      for {
        username <- sys.env.get("HEROKU_USERNAME")
        password <- sys.env.get("HEROKU_PASSWORD")
      } yield {
        val f = TestHerokuAPI.getApiKey(username, password)
        f.onFailure { case exp => fail(exp) }
        f.futureValue
      }
    }
    maybeAuthKey must be('defined)
    testCode(maybeAuthKey)
  }

  "login with valid credentials" must {
    "add the auth key to the session" in withLogin(new File("foo")) { maybeApiKey =>
      maybeApiKey must be('defined)
    }
  }

  def withApp(testCode: Option[(String, TestHerokuAPI.App, File)] => Any) = {
    val appDir = new File(sys.props("java.io.tmpdir"), System.nanoTime().toString)
    withLogin(appDir) { maybeApiKey =>
      val maybeApiKeyAndApp = maybeApiKey.map { apiKey =>
        val herokuApp = TestHerokuAPI.createApp()(apiKey.asApiKey).futureValue
        (apiKey, herokuApp, appDir)
      }
      try {
        testCode(maybeApiKeyAndApp)
      } finally {
        maybeApiKeyAndApp.map {
          case (apiKey, herokuApp, tmpAppDir) =>
            implicit val appName = herokuApp.name.asAppName
            implicit val apiKeyVal = apiKey.asApiKey

            TestHerokuAPI.destroyApp()
            tmpAppDir.delete()
        }
      }
    }
  }

  "createApp" must {
    "create an app on Heroku" in withApp { maybeAuthAndApp =>
      maybeAuthAndApp must be('defined)
      ()
    }
  }

  "getApps" must {
    "fail with an invalid apikey" in withApp { maybeAuthAndApp =>
      maybeAuthAndApp.map {
        case (apiKey, herokuApp, appDir) =>
          implicit val appName = herokuApp.name.asAppName
          implicit val apiKeyVal = apiKey.asApiKey

          TestHerokuAPI.getApps.onFailure { case _ => fail("should fail") }
      }
      ()
    }

    "work with an apikey" in withApp { maybeAuthAndApp =>
      maybeAuthAndApp.map {
        case (apiKey, herokuApp, appDir) =>
          implicit val appName = herokuApp.name.asAppName
          implicit val apiKeyVal = apiKey.asApiKey

          TestHerokuAPI.getApps.futureValue.size must be > 0
      }
      ()
    }

    "get the app that was created" in withApp { maybeAuthAndApp =>
      maybeAuthAndApp.map {
        case (apiKey, herokuApp, appDir) =>
          implicit val appName = herokuApp.name.asAppName
          implicit val apiKeyVal = apiKey.asApiKey

          val allHerokuApps = TestHerokuAPI.getApps.futureValue
          allHerokuApps.size must be > 0
          val newHerokuApp = allHerokuApps.filter(_.name == herokuApp.name)
          newHerokuApp.size must equal(1)
      }
      ()
    }
  }

  // clean up the app
  "logs" must {
    "get the log stream" in withApp { maybeAuthAndApp =>
      maybeAuthAndApp.map {
        case (apiKey, herokuApp, appDir) =>
          implicit val appName = herokuApp.name.asAppName
          implicit val apiKeyVal = apiKey.asApiKey

          // Wait here because it takes a few for the Heroku logplex to be enabled
          Thread.sleep(5000)
          (TestHerokuAPI.logs.futureValue \ "logplex_url").asOpt[String] must be('defined)
      }
    }
    ()
  }

  "restartDyno" must {
    "restart a Heroku dyno" in withApp {
      case Some((apiKey, herokuApp, appDir)) =>
        implicit val appName = herokuApp.name.asAppName
        implicit val apiKeyVal = apiKey.asApiKey

        TestHerokuAPI.dynoRestart("Dino")

      case None =>
        info("No app?!")
    }
    ()
  }

  // todo: test createSlug

  // todo: test buildSlug

  // todo: test appSetup

  /*
  "deploy" must {
    "deploy the app on Heroku" in withApp { maybeAuthAndApp =>
      maybeAuthAndApp.map {
        case (apiKey, herokuApp, appDir) =>
          import java.io._

          val indexWriter = new PrintWriter(new File(appDir, "index.php"))
          indexWriter.write("hello, world")
          indexWriter.close()

          val appJsonWriter = new PrintWriter(new File(appDir, "app.json"))
          appJsonWriter.write("{}")
          appJsonWriter.close()

          TestHerokuAPI

          // Wait here because the build & release will take a little bit
          Thread.sleep(30000)

          /*
          val deployedResult = await(WS.url(herokuApp.web_url).get())

          deployedResult.status must equal(OK)
          deployedResult.body must equal("hello, world")
          */
      }
      ()
    }
  }
  */
}
