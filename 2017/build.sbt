
name := "Advent of Code 2017"

lazy val root = (project in file("."))
  .settings(
    version := "0.0.1",
    scalaVersion := "2.12.4",
    scalacOptions ++= Seq(
      "-encoding",
      "UTF-8",
      "-deprecation",
      "-unchecked",
      "-feature",
      "-Ywarn-adapted-args",
      "-Ywarn-inaccessible",
      "-Ywarn-unused",
      "-Ywarn-dead-code",
      "-Ywarn-unused-import",
      "-Ywarn-value-discard",
      "-Xfatal-warnings"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.0.0-MF",
      "com.github.scopt" %% "scopt" % "3.7.0"
    ))
