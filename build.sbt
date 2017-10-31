// format: off
name := "forkpin"

organization := "com.github.synesso"

version := sys.env.getOrElse("TRAVIS_BRANCH", "dev")

scalaVersion := "2.12.4"

libraryDependencies ++= List(
  "org.specs2" %% "specs2-core" % "4.0.0" % "test",
  "org.specs2" %% "specs2-scalacheck" % "4.0.0" % "test",
  "org.bouncycastle" % "bcpg-jdk16" % "1.46"
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
