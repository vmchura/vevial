package algorithms

import io.vmchura.vevial.EjeVialBuilder.LandXMLToEje
import io.vmchura.vevial.EjeVialUtil.Progresiva
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.{EfficientEjeProgresiva, TEfficientSeqEjeElementsProgresiva}
import io.vmchura.vevial.relevamiento.SurveyGPX
import models.ProgresivaMilliseconds
import org.scalatest.flatspec.AnyFlatSpec

import java.time.ZonedDateTime
import scala.io.Codec
import scala.reflect.io.File
import scala.xml.XML
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import io.vmchura.vevial.elementdata.GPXElementData

import scala.concurrent.duration._

class SurveyProcessorOpsTest extends AnyFlatSpec {
  import algorithms.SurveyProcessorOps.SurveyProcessorOps

  "Create ProgressDistanceTimeMilliseconds" should "Create correctly" in {
    val tramoPath = getClass.getClassLoader.getResource("tramo1.xml").getPath
    val axisFile = File(tramoPath)
    val ejeEither: Either[Exception, EfficientEjeProgresiva] = new LandXMLToEje(axisFile.reader(Codec("UTF-8"))).toEje

    assert(ejeEither.isRight)
    val gpxSource = getClass.getClassLoader.getResource("001.gpx").getPath
    val surveyEither = SurveyGPX(gpxSource)
    assert(surveyEither.isRight)
    for {
      roadAxis <- ejeEither
      survey <- surveyEither
    } yield {
      val listProgressDistanceTimeStamp = survey.fillProgressDistance(roadAxis)
      val progressDistanceTime: List[(Progresiva, Int)] = MarkSequentialByStepTime.
        buildProgresiveByStepTime(listProgressDistanceTimeStamp, survey.duration, 100.millis).map(pdt => (pdt._1, pdt._2))

      def calculateProgresive(point: TPoint): Option[Double] = {
        roadAxis.projectPoint(point).map(pp => roadAxis.calcProgresive(pp))
      }

      def toProgressDistance(gpxData: GPXElementData): (Option[Progresiva], Option[ZonedDateTime]) = {
        val progressDistance = for {
          originPoint <- gpxData.point
          prog <- calculateProgresive(originPoint.value)
        } yield {
          Progresiva(prog.toInt)
        }
        val zonedDateTime = gpxData.zonedTime
        (progressDistance, zonedDateTime)
      }

      val progressDistanceSurvey = survey.surveyInformation.map(toProgressDistance)
//      progressDistanceSurvey.zipAll(progressDistanceTime, (None, None), (Progresiva(0), 0)).foreach{
//        case ((a, b), (x, y)) => {
//          println(s"${b.map(_.toString).getOrElse("")}\t${a.map(_.progresiva.toString).getOrElse("")}\t$y\t${x.progresiva}\t")
//        }
//      }
    }
  }

}
