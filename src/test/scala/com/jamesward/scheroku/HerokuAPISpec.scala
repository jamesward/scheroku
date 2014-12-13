package com.jamesward.scheroku

import HerokuClient._
import HerokuTest._
import play.api.libs.json.Json
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.ws.WSResponse

class HerokuAPISpec extends HerokuTest {
  "json conversion" must {
    "work" in {
      val herokuAppName = HerokuAppName("testApp")
      val jsonName = Json.toJson(herokuAppName)
      val herokuAppName2 = Json.fromJson[HerokuAppName](jsonName).get
      assert(herokuAppName==herokuAppName2)

      val herokuApp = HerokuApp(herokuAppName, "http://blah.com")
      val jsonApp = Json.toJson(herokuApp)
      val herokuApp2: HerokuApp = Json.fromJson[HerokuApp](jsonApp).get
      assert(herokuApp.name==herokuApp2.name)
      assert(herokuApp.web_url==herokuApp2.web_url)
    }
  }

  "login with invalid credentials" must {
    "fail" in {
      getApiKey("foo@foo.com", "bar").onSuccess { case _ => fail("should fail") }
    }
  }

  "login with valid credentials" must {
    "add the auth key to the session" in {
      withLogin { herokuApiKey =>
        herokuApiKey.apiKey.length must be > 0
      }
      ()
    }
  }

  "createApp" must {
    "create an app on Heroku and find it" in withApp { testParams =>
      implicit val apiKey = testParams.apiKey
      HerokuApp.get(testParams.herokuApp.name) foreach { maybeApp =>
        maybeApp must be('defined)
      }
    }
  }

  "get all HerokuApps" must {
    "fail with an invalid apikey" in withApp { testParams =>
      implicit val appName = testParams.herokuApp.name
      implicit val apiKey = testParams.apiKey
      HerokuApp.getAll.onFailure { case _ => fail("should fail") }
    }

    "work with an APIKey" in withApp { testParams =>
      implicit val appName = testParams.herokuApp.name
      implicit val apiKeyVal = testParams.apiKey
      HerokuApp.getAll.futureValue.size must be > 0
    }

    "get the app that was created" in withApp { testParams =>
      implicit val appName = testParams.herokuApp.name
      implicit val apiKeyVal = testParams.apiKey

      val allHerokuApps: Seq[HerokuApp] = HerokuApp.getAll.futureValue
      allHerokuApps.size must be > 0
      val newHerokuApp = allHerokuApps.filter(_.name == testParams.herokuApp.name)
      newHerokuApp.size must equal(1)
    }
  }

  "config vars" must {
    "be manipulable" in withApp { testParams =>
      implicit val appName: HerokuAppName = testParams.herokuApp.name
      implicit val apiKeyVal: HerokuApiKey = testParams.apiKey

      assert(testParams.herokuApp.configVars.futureValue.vars == Map.empty[String, String])

      val configVars1 = Map("name1" -> "value1")
      whenReady(testParams.herokuApp.addConfigVars(ConfigVars(configVars1))) { _ =>
        assert(testParams.herokuApp.configVars.futureValue.vars == configVars1)
      }

      val configVars2 = Map("name2" -> "value2")
      whenReady(testParams.herokuApp.addConfigVars(ConfigVars(configVars2))) { _ =>
        assert(testParams.herokuApp.configVars.futureValue.vars == configVars1 ++ configVars2)
      }

      whenReady(testParams.herokuApp.clearConfigVars()) { _ =>
        assert(testParams.herokuApp.configVars.futureValue.vars == Map.empty[String, String])
      }

      val configVars3 = Map("name3" -> "value3")
      whenReady(testParams.herokuApp.setConfigVars(ConfigVars(configVars3))) { _ =>
        assert(testParams.herokuApp.configVars.futureValue.vars == configVars3)
      }

      val configVarsAll = configVars1 ++ configVars2 ++ configVars3
      whenReady(testParams.herokuApp.configVars = ConfigVars(configVarsAll)) { _ =>
        assert(testParams.herokuApp.configVars.futureValue.vars == configVarsAll)
      }
    }
  }

  "logs" must {
    "get the log stream" in withApp { testParams =>
      implicit val appName = testParams.herokuApp.name
      implicit val apiKeyVal = testParams.apiKey

      // Wait here because it takes a few for the Heroku logplex to be enabled
      Thread.sleep(5000)
      (TestHerokuAPI.logs.futureValue \ "logplex_url").asOpt[String] must be('defined)
    }
  }

  "dynos" must {
    "go through lifecycle" in withApp { testParams =>
      implicit val appName = testParams.herokuApp.name
      implicit val apiKeyVal = testParams.apiKey

      val wsResponse1: WSResponse = testParams.herokuApp.createDyno("bash").futureValue
      assert(happyStatus(wsResponse1), s"Dyno creation failed: ${wsResponse1.body}")
      val dynoId = (wsResponse1.json \ "id").toString().replace("\"", "") // breakpoint here shows no dyno in https://dashboard.heroku.com/apps !!!

      val wsResponse2: WSResponse = Dyno(dynoId).restart.futureValue
      assert(happyStatus(wsResponse2), "Dyno failed to restart")      // this gives a valid response, but why? I can't see any dyno
    } // Heroku app is not deleted. Cannot understand why
  }

  "slugs" must {
    "go through lifecycle" in withApp { testParams =>
      implicit val apiKey = testParams.apiKey
      implicit val herokuApp = testParams.herokuApp
      implicit val appName = herokuApp.name
      val urlStr = "https://github.com/mslinn/tinyPlay23.git"
      gitHeadSHA(urlStr) foreach { sha =>
        val slug = Slug.build(urlStr).futureValue
        assert(slug.sourceUrl.toString == urlStr)
      }
    }
  }

  /** @return SHA of HEAD for Heroku slug creation */
  def gitHeadSHA(urlStr: String): Option[String] = {
    import sys.process._

    Process(List("git", "ls-remote", urlStr)).!!.trim
      .split("\n").find(_.contains("HEAD")).headOption.map(_.split("\t").head)
  }

  // todo: test createSlug

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
