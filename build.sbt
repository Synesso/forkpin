// format: off
name := "forkpin"

organization := "com.github.synesso"

version := sys.env.getOrElse("TRAVIS_BRANCH", "dev")

scalaVersion := "2.11.11"

libraryDependencies ++= List(
  "org.specs2" %% "specs2-core" % "3.9.1" % "test",
  "org.specs2" %% "specs2-scalacheck" % "3.9.1" % "test"
)

scalacOptions ++= Seq(
  "-Yrangepos",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:postfixOps",
  "-encoding",
  "UTF-8",
  "-target:jvm-1.8"
)

fork := true
