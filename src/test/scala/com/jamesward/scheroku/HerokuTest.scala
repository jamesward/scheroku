package com.jamesward.scheroku

import java.io.File
import org.scalatest.{WordSpec, MustMatchers}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}
import scala.concurrent.ExecutionContext.Implicits.global
import HerokuClient._

object TestHerokuAPI extends HerokuAPI {
  override val ec = global
}

object HerokuTest {
  import play.api.libs.ws.WSResponse

  def happyStatus(wsResult: WSResponse): Boolean = {
    val status = wsResult.status
    status>=200 && status<400
  }
}

trait HerokuTest extends WordSpec with MustMatchers with ScalaFutures with HerokuApiImplicits {
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(Span(30, Seconds))

  case class TestParams(apiKey: HerokuApiKey, herokuApp: HerokuApp, appDir: File)

  def withApp(testCode: TestParams => Any): Unit = {
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
