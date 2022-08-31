ThisBuild / version := "1.13"
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / organization := "io.vmchura"
lazy val ejevial = (project in file("ejevial")).settings(commonSettings).settings(
  name := "ejevial",
  libraryDependencies ++= List(
    "com.github.workingDog" %% "scalakml" % "1.5"
  )
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
    "com.norbitltd" %% "spoiwo" % "2.2.1",
    // https://mvnrepository.com/artifact/net.sf.supercsv/super-csv
    "net.sf.supercsv" % "super-csv" % "2.4.0"

  )


).dependsOn(ejevial,relevamientodata)


lazy val ejebuilder = (project in file("ejebuilder")).settings(commonSettings).settings(
  name := "ejebuilder",
).dependsOn(ejevial,relevamientodata,tesis)

lazy val ejebuilderview = (project in file("ejebuilderview")).settings(commonSettings ++ windowSettings).settings(
  name := "ejebuilderview"
).dependsOn(ejebuilder, ejevialview)



lazy val tesis = (project in file("tesisanalytics")).settings(commonSettings).settings(
  name := "tesisanalytics",
  libraryDependencies ++= List(
    //R https://github.com/dbdahl/rscala
    "org.ddahl" %% "rscala" % "3.2.18",
    //json
    "com.lihaoyi" %% "upickle" % "0.9.5",//
    // Last stable release
    "org.scalanlp" %% "breeze" % "1.0",

    // Native libraries are not included by default. add this if you want them
    // Native libraries greatly improve performance, but increase jar sizes.
    // It also packages various blas implementations, which have licenses that may or may not
    // be compatible with the Apache License. No GPL code, as best I know.
    "org.scalanlp" %% "breeze-natives" % "1.0",

    // The visualization library is distributed separately as well.
    // It depends on LGPL code
    "org.scalanlp" %% "breeze-viz" % "1.0"

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
  "org.openjfx" % s"javafx-$m" % "14.0.1" classifier "linux"
)},
//---------------- FIN SCALAFX
  libraryDependencies += "org.scalafx" %% "scalafx" % "14-R19",
  fork := true

  // compiles, but does not run
  // unmanagedJars in Compile += file(Path.userHome+"/Documents/002.DescargasFirefox/arcgis-java-100.6.0.jar")

)


//onLoad in Global := (onLoad in Global).value andThen {s: State => "project core" :: s}

