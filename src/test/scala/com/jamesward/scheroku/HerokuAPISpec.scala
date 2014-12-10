package com.jamesward.scheroku

import java.io.File

import org.scalatest.time.{Seconds, Span}
import org.scalatest.{WordSpec, MustMatchers}
import org.scalatest.concurrent.ScalaFutures

import play.api.libs.json.Json

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.ws.WSResponse

object TestHerokuAPI extends HerokuAPI {
  override val ec = global
}

object HerokuAPISpec {
  def happyStatus(wsResult: WSResponse): Boolean = {
    val status = wsResult.status
    status>=200 && status<400
  }
}

class HerokuAPISpec extends WordSpec with MustMatchers with ScalaFutures with HerokuApiImplicits {
  import HerokuApp._
  import HerokuAppName._
  import HerokuAPISpec._

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(Span(30, Seconds))

  "json conversion" must {
    "work" in {
      val herokuAppName = HerokuAppName("testApp")
      val jsonName = Json.toJson(herokuAppName)
      val herokuAppName2 = Json.fromJson[HerokuAppName](jsonName).get
      assert(herokuAppName==herokuAppName2)

      val herokuApp = HerokuApp(herokuAppName, "http://blah.com")(scala.concurrent.ExecutionContext.Implicits.global)
      val jsonApp = Json.toJson(herokuApp)
      val herokuApp2: HerokuApp = Json.fromJson[HerokuApp](jsonApp).get
      assert(herokuApp.appName==herokuApp2.appName)
      assert(herokuApp.web_url==herokuApp2.web_url)
    }
  }

  "login with invalid credentials" must {
    "fail" in {
      HerokuAPI.getApiKey("foo@foo.com", "bar").onSuccess { case _ => fail("should fail") }
    }
  }

  def withLogin(appDir: File)(testCode: Option[String] => Unit) = {
    val netRC = new File(System.getProperty("user.home"), ".netrc")
    val maybeAuthKey: Option[String] = if (netRC.exists) {
      import imagej.updater.webdav.NetrcParser
      import imagej.updater.webdav.NetrcParser.Credentials

      val credentials: Credentials = new NetrcParser().getCredentials("api.heroku.com")
      val f = HerokuAPI.getApiKey(credentials.getUsername, credentials.getPassword)
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
        val f = HerokuAPI.getApiKey(username, password)
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

  def withApp(testCode: Option[(String, HerokuApp, File)] => Any) = {
    val appDir = new File(sys.props("java.io.tmpdir"), System.nanoTime().toString)
    withLogin(appDir) { maybeApiKey =>
      val maybeApiKeyAndApp = maybeApiKey.map { apiKey =>
        implicit val apiKey2 = apiKey.asApiKey
        val herokuApp = HerokuApp.create().futureValue
        (apiKey, herokuApp, appDir)
      }
      try {
        testCode(maybeApiKeyAndApp)
        ()
      } finally {
        maybeApiKeyAndApp.map {
          case (apiKey, herokuApp, tmpAppDir) =>
            implicit val appName: HerokuAppName = herokuApp.appName
            implicit val apiKeyVal = apiKey.asApiKey

            TestHerokuAPI.destroyApp()
            tmpAppDir.delete()
        }
        ()
      }
    }
  }

  "createApp" must {
    "create an app on Heroku" in withApp { maybeAuthAndApp =>
      maybeAuthAndApp must be('defined)
    }
  }

  "get all HerokuApps" must {
    "fail with an invalid apikey" in withApp { maybeAuthAndApp =>
      maybeAuthAndApp.map {
        case (apiKey, herokuApp, appDir) =>
          implicit val appName = herokuApp.appName
          implicit val apiKeyVal = apiKey.asApiKey

          HerokuApp.getAll.onFailure { case _ => fail("should fail") }
      }
    }

    "work with an APIKey" in withApp { maybeAuthAndApp =>
      maybeAuthAndApp.map {
        case (apiKey, herokuApp, appDir) =>
          implicit val appName = herokuApp.appName
          implicit val apiKeyVal = apiKey.asApiKey

          HerokuApp.getAll.futureValue.size must be > 0
      }
    }

    "get the app that was created" in withApp { maybeAuthAndApp =>
      maybeAuthAndApp.map {
        case (apiKey, herokuApp, appDir) =>
          implicit val appName = herokuApp.appName
          implicit val apiKeyVal = apiKey.asApiKey

          val allHerokuApps: Seq[HerokuApp] = HerokuApp.getAll.futureValue
          allHerokuApps.size must be > 0
          val newHerokuApp = allHerokuApps.filter(_.appName == herokuApp.appName)
          newHerokuApp.size must equal(1)
      }
    }
  }

  "config vars" must {
    "be manipulable" in withApp { maybeAuthAndApp =>
      import HerokuApp._

      type StringMap = Map[String, String]
      maybeAuthAndApp.map {
        case (apiKey, herokuApp, appDir) =>
          implicit val appName = herokuApp.appName
          implicit val apiKeyVal = apiKey.asApiKey

          assert(Json.fromJson[StringMap](herokuApp.configVars.futureValue).get == Map.empty[String, String])

          val configVars1 = Map("name1" -> "value1", "name2" -> "value2")
          herokuApp.configVars = configVars1
          assert(Json.fromJson[StringMap](herokuApp.configVars.futureValue).get == configVars1) // Map() did not equal Map("name1" -> "value1", "name2" -> "value2")

          val configVars2 = Map("name3" -> "value3")
          assert(Json.fromJson[StringMap](herokuApp.configVars.futureValue).get == configVars2) // Not sure if this will append or replace

          val configVarsAll = configVars1 ++ configVars2
          assert(Json.fromJson[StringMap](herokuApp.configVars.futureValue).get == configVarsAll)
      }
    }
  }

  "logs" must {
    "get the log stream" in withApp { maybeAuthAndApp =>
      maybeAuthAndApp.map {
        case (apiKey, herokuApp, appDir) =>
          implicit val appName = herokuApp.appName
          implicit val apiKeyVal = apiKey.asApiKey

          // Wait here because it takes a few for the Heroku logplex to be enabled
          Thread.sleep(5000)
          (TestHerokuAPI.logs.futureValue \ "logplex_url").asOpt[String] must be('defined)
      }
    }
  }

  "dynos" must {
    "go through lifecycle" in withApp {
      case Some((apiKey, herokuApp, appDir)) =>
        implicit val appName = herokuApp.appName
        implicit val apiKeyVal = apiKey.asApiKey

        val wsResponse1: WSResponse = herokuApp.createDyno("Dino").futureValue
        assert(happyStatus(wsResponse1), s"Dino creation failed: ${wsResponse1.body}")
        val dynoId = (wsResponse1.json \ "id").toString().replace("\"", "")

        val wsResponse2: WSResponse = Dyno(dynoId).restart.futureValue
        assert(happyStatus(wsResponse2), "Dino failed to restart")

      case None =>
        fail("No app?!")
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
      ()
    }
  }
  */
}
