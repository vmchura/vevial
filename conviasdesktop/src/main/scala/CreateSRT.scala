import io.vmchura.vevial.EjeVialUtil.Progresiva
import models.ProgresivaMilliseconds
import org.jcodec.api.{FrameGrab, PictureWithMetadata}
import org.jcodec.api.awt.AWTSequenceEncoder
import org.jcodec.common.io.NIOUtils
import org.jcodec.common.model.{Picture, Rational}
import org.jcodec.scale.AWTUtil

import java.awt.image.ImageObserver
import javax.imageio.ImageIO
import java.awt.{Color, Font, Image}
import java.io
import java.io.{BufferedWriter, FileWriter}
import java.security.Timestamp
import java.text.SimpleDateFormat
import java.time.{LocalDateTime, LocalTime, ZoneOffset, ZonedDateTime}
import java.util.{Date, TimeZone}
import scala.Console.println
import scala.concurrent.Await
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

  def buildSubtitles(pathVideo: String, gpxXML: Node, ejeXMLFile: File, tramoName: String): Either[Exception, Seq[String]] = {
    GpxToUTM.parse(gpxXML, ejeXMLFile).map { progresivasTimeStamp =>
      val file = new java.io.File(pathVideo)
      val grab = FrameGrab.createFrameGrab(NIOUtils.readableChannel(file))
      val totalDuration = grab.getVideoTrack.getMeta.getTotalDuration.toInt * 1000
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
            |${tramoName}
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
  def execute(pathVideo: String, gpxXML: Node, ejeXMLFile: File,  outputPath: String, tramoName: String): Unit = {
    buildSubtitles(pathVideo, gpxXML, ejeXMLFile, tramoName).map{ subtitles =>
      writeSubtitles(subtitles, outputPath)
    }
  }

  println(args.map(arg => s"[$arg]").mkString(","))
  val tramoFile = File(args(1))
  val gpxNode = XML.load(args(2))
  val pathVideo = args(0)
  CreateSRT.execute(pathVideo, gpxNode, tramoFile, args(3), args(4))
}
