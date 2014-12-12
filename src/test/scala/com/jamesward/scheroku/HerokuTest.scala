package com.jamesward.scheroku

import java.io.File
import org.scalatest.{WordSpec, MustMatchers}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}
import scala.concurrent.ExecutionContext.Implicits.global

object TestHerokuAPI extends HerokuAPI {
  override val ec = global
}

trait HerokuTest extends WordSpec with MustMatchers with ScalaFutures with HerokuApiImplicits {
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(Span(30, Seconds))

  def withApp(testCode: Option[(String, HerokuApp, File)] => Any) = {
    val now = System.nanoTime.toString
    val appDir = new File(sys.props("java.io.tmpdir"), now)
    withLogin(appDir) { maybeApiKey =>
      val maybeApiKeyAndApp: Option[(String, HerokuApp, File)] = maybeApiKey.map { apiKey =>
        implicit val apiKey2 = apiKey.asApiKey
        val herokuApp = HerokuApp.create(Some(s"test$now")).futureValue
        //println(s"Testing with new Heroku app $herokuApp")
        (apiKey, herokuApp, appDir)
      }
      try {
        testCode(maybeApiKeyAndApp)
        ()
      } finally {
        maybeApiKeyAndApp.map {
          case (apiKey, herokuApp, tmpAppDir) =>
            implicit val appName: HerokuAppName = herokuApp.name
            implicit val apiKeyVal = apiKey.asApiKey

            TestHerokuAPI.destroyApp()
            tmpAppDir.delete()
        }
        ()
      }
    }
  }

  def withLogin(appDir: File)(testCode: Option[String] => Unit) = {
    val netRC = new File(System.getProperty("user.home"), ".netrc")
    val maybeAuthKey: Option[String] = if (netRC.exists) {
      import imagej.updater.webdav.NetrcParser
      import imagej.updater.webdav.NetrcParser.Credentials

      val credentials: Credentials = new NetrcParser().getCredentials("api.heroku.com")
      val f = getApiKey(credentials.getUsername, credentials.getPassword)
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
        val f = getApiKey(username, password)
        f.onFailure { case exp => fail(exp) }
        f.futureValue
      }
    }
    maybeAuthKey must be('defined)
    testCode(maybeAuthKey)
  }
}
