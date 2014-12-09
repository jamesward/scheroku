name := "scheroku"
organization := "com.jamesward"
version      := "0.0.1"

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

libraryDependencies ++= Seq(
  "org.apache.commons"  % "commons-compress" % "1.8.1" withSources(),
  "com.typesafe.play"  %% "play-json"        % "2.3.7" withSources(),
  "com.typesafe.play"  %% "play-ws"          % "2.3.7" withSources(),
  "com.typesafe.akka"  %% "akka-actor"       % "2.3.4" withSources(),
  //
  "org.scalatest"      %% "scalatest"        % "2.2.1" % "test" withSources()
)

scalaVersion := "2.11.4"
scalacOptions ++= Seq("-deprecation", "-encoding", "UTF-8", "-feature", "-target:jvm-1.7", "-unchecked",
  "-Ywarn-adapted-args", "-Ywarn-value-discard", "-Xlint")
javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked", "-source", "1.7", "-target", "1.7", "-g:vars")

bintraySettings
publishMavenStyle := false

com.typesafe.sbt.SbtGit.versionWithGit

logBuffered in Test := false
Keys.fork in Test := false
parallelExecution in Test := false
logLevel in test := Level.Info // Level.INFO is needed to see detailed output when running tests
