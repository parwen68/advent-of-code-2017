enablePlugins(ScalaJSPlugin)

name := "advent-of-code-2017"

version := "1.0"

scalaVersion in ThisBuild := "2.12.4"

lazy val root = project.in(file(".")).
  aggregate(adocJS, adocJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val adoc = crossProject.in(file(".")).
  settings(
    name := "advent-of-code",
    version := "1.0-SNAPSHOT",
    libraryDependencies += {
      "org.scalatest" %% "scalatest" % "3.0.4" % "test"
    }
  ).
  jvmSettings(
    mainClass in Compile := Some("se.parwen.adoc.day13.Main")
  ).
  jsSettings(
    //scalaJSUseMainModuleInitializer := true
  )

lazy val adocJVM = adoc.jvm
lazy val adocJS = adoc.js