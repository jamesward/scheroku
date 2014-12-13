package com.jamesward.scheroku

import java.io.File
import HerokuClient._
import org.scalatest.{WordSpec, MustMatchers}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import scala.concurrent.ExecutionContext

object TestHerokuAPI extends HerokuAPI

object HerokuTest {
  import play.api.libs.ws.WSResponse

  def happyStatus(wsResult: WSResponse): Boolean = {
    val status = wsResult.status
    status>=200 && status<400
  }
}

trait HerokuTest extends WordSpec with MustMatchers with ScalaFutures with IntegrationPatience with HerokuApiImplicits {
  case class TestParams(apiKey: HerokuApiKey, herokuApp: HerokuApp, appDir: File)

  def withApp(testCode: TestParams => Any)(implicit ec: ExecutionContext): Unit = {
    val now = System.nanoTime.toString
    val appDir = new File(sys.props("java.io.tmpdir"), now)
    withLogin { implicit apiKey: HerokuApiKey =>
      implicit val herokuApp: HerokuApp = HerokuApp.create(Some(s"test$now")).futureValue
      implicit val appName: HerokuAppName = herokuApp.name
      //println(s"Testing with new Heroku app $herokuApp")
      try {
        testCode(TestParams(apiKey, herokuApp, appDir))
        ()
      } finally {
        TestHerokuAPI.destroyApp()
        appDir.delete()
        ()
      }
    }.futureValue
  }
}
