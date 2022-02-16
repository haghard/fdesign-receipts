import sbt._

lazy val `fdesign-receipts` = (project in file(".")).
  settings (
    name := "fdesign-receipts",
    organization := "none",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.13.8"
  )

scalacOptions ++= Seq(
  "-language:higherKinds",
  "-language:existentials",
  "-Yrepl-class-based",
  "-deprecation",
  "-explaintypes",
  "-Yrangepos",
  "-feature",
  "-unchecked",
  "-Xlint:_,-type-parameter-shadow",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-opt-warnings",
  "-Ywarn-extra-implicit",
  "-Ywarn-unused:_,imports",
  "-Ywarn-unused:imports",
  "-opt:l:inline",
  "-opt-inline-from:<source>"
)

val ZIOVersion = "1.0.7"
val AmmoniteVersion = "2.5.2"

libraryDependencies ++= Seq(
  //for intro only
  "dev.zio" %% "zio" % ZIOVersion,
  "com.lihaoyi" % "ammonite" % AmmoniteVersion % "test" cross CrossVersion.full
)

scalafmtOnCompile := true

addCommandAlias("c", "compile")
addCommandAlias("r", "reload")

//fork in run := false

// ammonite repl
sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue

