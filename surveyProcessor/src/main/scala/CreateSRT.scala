import algorithms.MarkSequentialByStepTime
import io.vmchura.vevial.EjeVialUtil.Progresiva
import io.vmchura.vevial.EjeVialBuilder.{LandXMLToEje, LandXMLWithRestrictionsToEje, LandXmlKmlToEje}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import models.ProgresivaMilliseconds
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import io.vmchura.vevial.PlanarGeometric.RestrictiveEje.ProgresivePoint
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point

import java.io.{BufferedWriter, FileWriter}
import java.text.SimpleDateFormat
import java.time.ZonedDateTime
import java.util.concurrent.TimeUnit
import java.util.{Date, Scanner, TimeZone}
import scala.Console.println
import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.io.Codec
import scala.reflect.io.File
import scala.xml.{Node, XML}

object CreateSRT extends App {

  def millisecondsToHMS(milliseconds: Int): String = {
    val seconds = milliseconds / 1000
    val sss = milliseconds % 1000
    val minutes = seconds / 60
    val ss = seconds % 60
    val hours = minutes / 60
    val mm = minutes % 60
    f"$hours%02d:$mm%02d:$ss%02d,$sss%03d"
  }

  def buildSubtitles(durationPath: String, gpxXML: Node, ejeEither: Either[Exception, EfficientEjeProgresiva], tramoName: String): Either[Exception, Seq[String]] = {
    GpxToUTM.parse(gpxXML, ejeEither).map { progresivasTimeStamp =>
      val file = new java.io.File(durationPath)
      val grab = scala.io.Source.fromFile(file)
      val totalDuration = (grab.getLines().toList.head.toDouble * 1000).toInt
      grab.close()
      val progresivesToWrite = MarkSequentialByStepTime.buildProgresiveByStepTime(progresivasTimeStamp, Duration(totalDuration, TimeUnit.MILLISECONDS), Duration(100, TimeUnit.MILLISECONDS))
      val dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
      dateFormatter.setTimeZone(TimeZone.getTimeZone("GMT-5"))

      progresivesToWrite.zipWithIndex.map{ case ((progresivaToWrite, milliSecond, timeZoned), index) =>
        val nextMillisecond = milliSecond + 100
       val progresivaHitos = Progresiva(progresivaToWrite.progresiva)
       val progresivaRealSTR = progresivaToWrite.show(withSpaces = true, withKmLeftPadding = 3)
       val progresivaHitosSTR = progresivaHitos.show(withSpaces = true, withKmLeftPadding = 3)
        val utcTime = dateFormatter.format(Date.from(timeZoned.toInstant))
        s"""|${index+1}
            |${millisecondsToHMS(milliSecond)} --> ${millisecondsToHMS(nextMillisecond)}
            |$tramoName
            |Hora aproximada: $utcTime
            |Prog aproximada: $progresivaRealSTR ($progresivaHitosSTR)""".stripMargin
      }
    }
  }
  def writeSubtitles(subtitles: Seq[String], outputPath: String): Unit = {
      val file = new java.io.File(outputPath)
      val bw = new BufferedWriter(new FileWriter(file))
      for (line <- subtitles) {
        bw.write(line)
        bw.write(System.lineSeparator())
        bw.write(System.lineSeparator())
      }
      bw.close()

  }
  def execute(durationPath: String, gpxXML: Node, ejeEither: Either[Exception, EfficientEjeProgresiva], outputPath: String, tramoName: String): Unit = {
    buildSubtitles(durationPath, gpxXML, ejeEither, tramoName).map{ subtitles =>
      writeSubtitles(subtitles, outputPath)
    }
  }
  val tramoFile = File(args(0))
  val restrictionsFile = File(args(1))
  val restrictions: List[ProgresivePoint] = restrictionsFile.lines().map{ line =>
    val Array(x,y, prog) = line.split(",").map(_.toDouble)
    new ProgresivePoint(Point(x,y), prog)
  }.toList

  val ejeEither: Either[Exception, EfficientEjeProgresiva] = new LandXMLWithRestrictionsToEje(tramoFile.reader(Codec("UTF-8")), restrictions
                                                                              //,kmlFile.reader(Codec("UTF-8"))
                                                                              ).toEje

  val scanner = new Scanner(System.in)
  while(scanner.hasNextLine){
    val line = scanner.nextLine()
    val Array(durationPath, gpxPath, pathOutput, tramoName) = line.split(";").map(_.trim)
    try {

      val gpxNode = XML.load(gpxPath)
      CreateSRT.execute(durationPath, gpxNode, ejeEither, pathOutput, tramoName)

    }catch{
      case e: Exception =>
    }
  }
  scanner.close()



}
