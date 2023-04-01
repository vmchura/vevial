package io.vmchura.vevial.relevamiento
import io.vmchura.vevial.elementdata.GPXElementData

import java.time.temporal.ChronoUnit
import java.util.concurrent.TimeUnit
import scala.xml.XML
import scala.concurrent.duration._
case class SurveyGPX(surveyInformation: List[GPXElementData]) extends Survey[GPXElementData] {
  val duration: Duration = {
    val zoned = surveyInformation.flatMap(_.zonedTime)
    Duration(ChronoUnit.MILLIS.between(zoned.last, zoned.head), TimeUnit.MILLISECONDS)
  }
}

object SurveyGPX {
  def apply(sourceFile: String): Either[List[String], SurveyGPX] = {
    try {
      val gpxRootNode = XML.load(sourceFile)
      val puntualData = gpxRootNode \\ "trkpt"
      Right(new SurveyGPX(puntualData.map(GPXElementData.apply).toList))
    } catch {
      case e: Exception => Left(List(e.getMessage))
    }
  }
}
