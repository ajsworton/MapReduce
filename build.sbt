lazy val commonSettings = Seq(
    organization := "com.takt",
    scalaVersion := "2.12.3",
    version      := "0.1.0-SNAPSHOT"
  )

lazy val acme = project.settings(commonSettings)

lazy val mapper = project.settings(commonSettings).dependsOn(acme)

lazy val reducer = project.settings(commonSettings).dependsOn(acme)

