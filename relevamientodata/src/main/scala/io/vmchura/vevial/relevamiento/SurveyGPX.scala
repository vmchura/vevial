package io.vmchura.vevial.relevamiento
import io.vmchura.vevial.elementdata.GPXElementData
import scala.xml.XML

case class SurveyGPX(elements: Seq[GPXElementData]) extends TSimpleRelevamiento[GPXElementData] {

  override def sliceBy(minX: Double, maxX: Double, minY: Double, maxY: Double): TSimpleRelevamiento[GPXElementData] = this
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
