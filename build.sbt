ThisBuild / version := "0.1"
lazy val scala212 = "2.13.0"
lazy val scala213 = "2.12.10"

lazy val supportedScalaVersions = List(scala212, scala213)



lazy val ejevial = (project in file("ejevial")).settings(commonSettings).settings(
  name := "ejevial",
  crossScalaVersions := supportedScalaVersions
)

lazy val ejevialview = (project in file("ejevialview")).settings(commonSettings ++ windowSettings).settings(
  name := "ejevialview",
  crossScalaVersions := List(scala213)
).dependsOn(ejevial,relevamientodata)

lazy val relevamientodata = (project in file("relevamientodata")).settings(commonSettings).settings(
  name := "relevamientodata",
  crossScalaVersions := supportedScalaVersions
).dependsOn(ejevial)

lazy val viricalc = (project in file("viricalc")).settings(commonSettings).settings(
  name := "viricalc",
  crossScalaVersions := supportedScalaVersions
  /*,
  libraryDependencies ++= List(
    "com.norbitltd" %% "spoiwo" % "1.6.1"
  )

   */
).dependsOn(ejevial,relevamientodata)
lazy val iricalculator = (project in file("iricalculator")).settings(commonSettings).settings(
  name := "relevamientodata",
  crossScalaVersions := supportedScalaVersions
).dependsOn(ejevial,relevamientodata)


lazy val ejebuilder = (project in file("ejebuilder")).settings(commonSettings).settings(
  name := "ejebuilder",
  crossScalaVersions := supportedScalaVersions
).dependsOn(ejevial,relevamientodata)

lazy val tesis = (project in file("tesisanalytics")).settings(commonSettings).settings(
  name := "tesisanalytics",
  libraryDependencies ++= List(
    //R
    "org.ddahl" %% "rscala" % "3.2.16",
    //json
    "com.lihaoyi" %% "upickle" % "0.8.0"
  ),
  crossScalaVersions := List(scala213)
).dependsOn(ejevial,relevamientodata)

lazy val core = (project in file(".")).settings(commonSettings).settings(
  name := "Vevial",
  libraryDependencies ++= List(),
  crossScalaVersions := List(scala212)
).dependsOn(ejevial,relevamientodata).aggregate(ejevial,relevamientodata)



lazy val commonSettings = Seq(

  organization := "io.vmchura",

  scalacOptions += "-feature",
  //scalatest
  libraryDependencies ++= List(
    "org.scalactic" %% "scalactic" % "3.0.8",
    "org.scalatest" %% "scalatest" % "3.0.8" % "test"),

    //cats
  libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0",

  //logging
  libraryDependencies ++= List("ch.qos.logback" % "logback-classic" % "1.2.3",
                               "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"),

  //xml
    libraryDependencies +="org.scala-lang.modules" %% "scala-xml" % "1.2.0"

)

lazy val windowSettings = Seq (
{
/**
  * SCALAFX Temporal
  * USING JAVA 12
  */

lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
libraryDependencies ++= javaFXModules.map( m =>
  "org.openjfx" % s"javafx-$m" % "12.0.2" classifier "linux"
)},
//---------------- FIN SCALAFX
  libraryDependencies += "org.scalafx" %% "scalafx" % "12.0.2-R18",
  resolvers += Resolver.sonatypeRepo("snapshots"),
  fork := true

  // compiles, but does not run
  // unmanagedJars in Compile += file(Path.userHome+"/Documents/002.DescargasFirefox/arcgis-java-100.6.0.jar")

)


//onLoad in Global := (onLoad in Global).value andThen {s: State => "project core" :: s}

