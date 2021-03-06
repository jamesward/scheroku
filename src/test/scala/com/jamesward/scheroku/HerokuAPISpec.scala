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

class HerokuAPISpec extends WordSpec with MustMatchers with ScalaFutures {

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(Span(30, Seconds))

  "login with invalid credentials" must {
    "fail" in {
      TestHerokuAPI.getApiKey("foo@foo.com", "bar").onComplete {
        case Success(_) => fail("should fail")
        case Failure(_) => // all good
      }
    }
  }

  def withLogin(appDir: File)(testCode: Option[String] => Any) = {
    assume(sys.env.get("HEROKU_USERNAME").isDefined)
    assume(sys.env.get("HEROKU_PASSWORD").isDefined)

    val maybeAuthKey = for {
      username <- sys.env.get("HEROKU_USERNAME")
      password <- sys.env.get("HEROKU_PASSWORD")
    } yield {
      val f = TestHerokuAPI.getApiKey(username, password)
      f.onComplete {
        case Success(_) => // cool
        case Failure(exp) => fail(exp)
      }
      f.futureValue
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

        val herokuApp = TestHerokuAPI.createApp(apiKey).futureValue

        (apiKey, herokuApp, appDir)
      }
      try {
        testCode(maybeApiKeyAndApp)
      } finally {
        maybeApiKeyAndApp.map {
          case (apiKey, herokuApp, tmpAppDir) =>
            TestHerokuAPI.destroyApp(apiKey, herokuApp.name)
            tmpAppDir.delete()
        }
      }
    }
  }

  "createApp" must {
    "create an app on Heroku" in withApp { maybeAuthAndApp =>
      maybeAuthAndApp must be('defined)
    }
  }

  "getApps" must {
    "fail with an invalid apikey" in withApp { maybeAuthAndApp =>
      maybeAuthAndApp.map {
        case (apiKey, herokuApp, appDir) =>
          TestHerokuAPI.getApps("asdf").onComplete {
            case Success(_) => fail("should fail")
            case Failure(_) => // all good
          }
      }
    }
    "work with an apikey" in withApp { maybeAuthAndApp =>
      maybeAuthAndApp.map {
        case (apiKey, herokuApp, appDir) =>
          TestHerokuAPI.getApps(apiKey).futureValue.size must be > 0
      }
    }
    "get the app that was created" in withApp { maybeAuthAndApp =>
      maybeAuthAndApp.map {
        case (apiKey, herokuApp, appDir) =>
          val allHerokuApps = TestHerokuAPI.getApps(apiKey).futureValue
          allHerokuApps.size must be > 0
          val newHerokuApp = allHerokuApps.filter(_.name == herokuApp.name)
          newHerokuApp.size must equal(1)
      }
    }
  }

  // cleanup the app
  "logs" must {
    "get the log stream" in withApp { maybeAuthAndApp =>
      maybeAuthAndApp.map {
        case (apiKey, herokuApp, appDir) =>
          // Wait here because it takes a few for logplex to be enabled
          Thread.sleep(5000)

          (TestHerokuAPI.logs(apiKey, herokuApp.name).futureValue \ "logplex_url").asOpt[String] must be('defined)
      }
    }
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
    }
  }
  */

}
