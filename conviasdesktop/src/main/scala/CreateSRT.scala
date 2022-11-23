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
import java.util.{Date, Scanner, TimeZone}
import scala.Console.println
import scala.annotation.tailrec
import scala.io.Codec
import scala.reflect.io.File
import scala.xml.{Node, XML}

object CreateSRT extends App {

  def calculateProgresiva(prevProgresiva: ProgresivaMilliseconds, currentProgresiva: ProgresivaMilliseconds, time: Long): Progresiva = {
    if (time < prevProgresiva.millisFromStart) {
      prevProgresiva.progresiva
    } else {
      if (time > currentProgresiva.millisFromStart) {
        currentProgresiva.progresiva
      } else {
        val aTime = time - prevProgresiva.millisFromStart
        val bProgresiva = currentProgresiva.progresiva.progresiva - prevProgresiva.progresiva.progresiva
        val bTime = currentProgresiva.millisFromStart - prevProgresiva.millisFromStart
        if (bTime == 0) {
          Progresiva(prevProgresiva.progresiva.progresiva)
        } else {
          val aProgresiva = aTime * bProgresiva / bTime
          Progresiva(prevProgresiva.progresiva.progresiva + aProgresiva.toInt)
        }
      }
    }
  }

  @tailrec
  def findProgresiva(progresivas: List[ProgresivaMilliseconds], lastProgresiva: ProgresivaMilliseconds, time: Long): (Progresiva, List[ProgresivaMilliseconds], ProgresivaMilliseconds) = {
    progresivas match {
      case Nil => (lastProgresiva.progresiva, progresivas, lastProgresiva)
      case current :: tail => if (time > current.millisFromStart) {
        findProgresiva(tail, current, time)
      } else {
        (calculateProgresiva(lastProgresiva, current, time), progresivas, lastProgresiva)
      }
    }
  }

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

      var currentList = progresivasTimeStamp
      var lastProgresiva = ProgresivaMilliseconds(Progresiva(0), -1L, ZonedDateTime.now())
      val dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
      dateFormatter.setTimeZone(TimeZone.getTimeZone("GMT-5"))

     (0 until totalDuration by 100).zipWithIndex.map{ case (milliSecond, index) =>
        val nextMillisecond = milliSecond + 100
        val (progresivaToWrite, newList, newLast) = findProgresiva(currentList, lastProgresiva, milliSecond)
        currentList = newList
        lastProgresiva = newLast
        val utcTime = dateFormatter.format(Date.from(lastProgresiva.timeZoned.toInstant))

        s"""|${index+1}
            |${millisecondsToHMS(milliSecond)} --> ${millisecondsToHMS(nextMillisecond)}
            |$tramoName
            |Hora aproximada: $utcTime
            |Prog aproximada: ${progresivaToWrite.show(withSpaces = true, withKmLeftPadding = 3)}""".stripMargin
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
  println(args.map(arg => s"[$arg]").mkString(","))
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
    println(List(durationPath, gpxPath, pathOutput, tramoName).mkString(" -- "))
    val gpxNode = XML.load(gpxPath)
    CreateSRT.execute(durationPath, gpxNode, ejeEither, pathOutput, tramoName)
  }
  scanner.close()



}
