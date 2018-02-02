
name := "Advent of Code 2017"

version := "0.0.1"
inThisBuild(Seq(
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.12.4-bin-typelevel-4"
))

scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Ypartial-unification",
  "-Ywarn-adapted-args",
  "-Ywarn-inaccessible",
  "-Ywarn-unused",
  "-Ywarn-dead-code",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard",
  "-Xfatal-warnings")

scalacOptions in (Compile, console) := Seq(
  "-encoding",
  "UTF-8",
  "-feature",
  "-Ypartial-unification"
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.0-MF",
  "org.typelevel" %% "kittens" % "1.0.0-RC2",
  "com.github.scopt" %% "scopt" % "3.7.0",
  "com.lihaoyi" %% "fastparse" % "1.0.0"
)
