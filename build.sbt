name := "scheroku"
organization := "com.jamesward"

// see https://github.com/heroku/sbt-heroku
//herokuAppName in Compile := "your-heroku-app-name"  // PUT YOUR HEROKU APP NAME HERE

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

libraryDependencies ++= Seq(
   "org.apache.commons"  % "commons-compress"               % "1.8.1" withSources(),
// "net.imagej"          % "imagej-plugins-uploader-webdav" % "0.2.0" withSources(),
   "com.typesafe.play"  %% "play-json"                      % "2.3.7" withSources(),
   "com.typesafe.play"  %% "play-ws"                        % "2.3.7" withSources(),
   "com.typesafe.akka"  %% "akka-actor"                     % "2.3.4" withSources(),

   "org.scalatest"      %% "scalatest"                      % "2.2.1" % "test" withSources()
)

resolvers ++= Seq("maven-decentral" at "http://maven-decentral.github.io/m2/releases")

scalaVersion := "2.11.4"
scalacOptions ++= Seq("-deprecation", "-encoding", "UTF-8", "-feature", "-target:jvm-1.7", "-unchecked",
  "-Ywarn-adapted-args", "-Ywarn-value-discard", "-Xlint")
javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked", "-source", "1.7", "-target", "1.7", "-g:vars")

bintraySettings
publishMavenStyle := false

site.settings
site.includeScaladoc()

com.typesafe.sbt.SbtGit.versionWithGit

logBuffered in Test := false
Keys.fork in Test := false
parallelExecution in Test := false
logLevel in test := Level.Info // Level.INFO is needed to see detailed output when running tests

// define the statements initially evaluated when entering 'console', 'console-quick', or 'console-project'
initialCommands := """import java.io.File
                     |import java.net.URL
                     |import com.jamesward.scheroku._
                     |import com.jamesward.scheroku.HerokuAPI._
                     |import com.jamesward.scheroku.HerokuApp._
                     |import com.jamesward.scheroku.HerokuAppName._
                     |import com.jamesward.scheroku.Dyno._
                     |import com.micronautics.scheroku.DynoSizeEnum
                     |import com.micronautics.scheroku.DynoSizeEnum._
                     |import play.api.libs.json.Json
                     |import play.api.libs.ws.WSResponse
                     |import imagej.updater.webdav.NetrcParser
                     |import scala.concurrent.ExecutionContext.Implicits.global
                     |""".stripMargin
