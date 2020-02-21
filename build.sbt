ThisBuild / version := "1.13"
ThisBuild / scalaVersion := "2.13.1"

lazy val ejevial = (project in file("ejevial")).settings(commonSettings).settings(
  name := "ejevial"
)

lazy val ejevialview = (project in file("ejevialview")).settings(commonSettings ++ windowSettings).settings(
  name := "ejevialview"
).dependsOn(ejevial,relevamientodata)

lazy val relevamientodata = (project in file("relevamientodata")).settings(commonSettings).settings(
  name := "relevamientodata",
  libraryDependencies += "net.sf.supercsv" % "super-csv" % "2.4.0"

).dependsOn(ejevial)

lazy val viricalc = (project in file("viricalc")).settings(commonSettings).settings(
  name := "viricalc",
  libraryDependencies ++= List(
    "com.norbitltd" %% "spoiwo" % "1.6.2-SNAPSHOT",
    // https://mvnrepository.com/artifact/net.sf.supercsv/super-csv
    "net.sf.supercsv" % "super-csv" % "2.4.0"

  )


).dependsOn(ejevial,relevamientodata)


lazy val ejebuilder = (project in file("ejebuilder")).settings(commonSettings).settings(
  name := "ejebuilder",
).dependsOn(ejevial,relevamientodata)

lazy val tesis = (project in file("tesisanalytics")).settings(commonSettings).settings(
  name := "tesisanalytics",
  libraryDependencies ++= List(
    //R https://github.com/dbdahl/rscala
    "org.ddahl" %% "rscala" % "3.2.18",
    //json
    "com.lihaoyi" %% "upickle" % "0.9.5"
  )
).settings(windowSettings).dependsOn(ejevial,relevamientodata)

lazy val core = (project in file(".")).settings(commonSettings).settings(
  name := "Vevial",
  libraryDependencies ++= List()
).dependsOn(ejevial,relevamientodata,viricalc).aggregate(ejevial,relevamientodata,viricalc)



lazy val commonSettings = Seq(

  organization := "io.vmchura",

  scalacOptions += "-feature",
  //scalatest
  libraryDependencies ++= List(
    "org.scalactic" %% "scalactic" % "3.1.1",
    "org.scalatest" %% "scalatest" % "3.1.1" % "test"),

    //cats
  libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.0",

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
  fork := true

  // compiles, but does not run
  // unmanagedJars in Compile += file(Path.userHome+"/Documents/002.DescargasFirefox/arcgis-java-100.6.0.jar")

)


//onLoad in Global := (onLoad in Global).value andThen {s: State => "project core" :: s}

