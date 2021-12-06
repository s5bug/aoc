lazy val aoc = (project in file(".")).settings(
  organization := "tf.bug",
  name := "aoc",
  version := "0.1.0",
  scalaVersion := "2.13.7",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.7.0",
    "org.typelevel" %% "cats-effect" % "3.3.0",
    "co.fs2" %% "fs2-core" % "3.2.0",
    "org.typelevel" %% "cats-parse" % "0.3.6",
    "org.typelevel" %% "spire" % "0.18.0-M2",
    "org.http4s" %% "http4s-core" % "1.0.0-M29",
    "org.http4s" %% "http4s-jdk-http-client" % "0.6.0-M6",
  ),
)
