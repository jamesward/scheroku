name := "scheroku"

organization := "com.jamesward"

git.baseVersion := "0.1"

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.3.2",
  "com.typesafe.play" %% "play-ws" % "2.3.2",
  "com.typesafe.akka" %% "akka-actor" % "2.3.4",
  "org.apache.commons" % "commons-compress" % "1.8.1",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)

publishMavenStyle := false

bintraySettings

com.typesafe.sbt.SbtGit.versionWithGit