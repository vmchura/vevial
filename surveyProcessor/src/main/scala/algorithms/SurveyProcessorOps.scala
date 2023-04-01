package algorithms

import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.TEfficientSeqEjeElementsProgresiva
import io.vmchura.vevial.elementdata.GPXElementData
import io.vmchura.vevial.relevamiento.Survey
import models.{AlgorithmFill, PointTimeStamp, ProgresivaMilliseconds, ProgresivaTimeStamp}

import java.time.temporal.ChronoUnit

object SurveyProcessorOps{
  implicit class SurveyProcessorOps(survey: Survey[GPXElementData]) {
    private def fillDates(efficientAxisProgressDistance: TEfficientSeqEjeElementsProgresiva): List[ProgresivaTimeStamp] = {
        val initialDataTimeStamp = survey.surveyInformation.map(_.zonedTime)
      survey.surveyInformation.zip(AlgorithmFill.completeTimeStamp(initialDataTimeStamp)).map{
          case (rawGeodesicOption, zonedDateTime) => PointTimeStamp(rawGeodesicOption.point.map(_.value), zonedDateTime)
        }.map(_.toProgressDistanceTimeStamp(efficientAxisProgressDistance))
    }
    def fillProgressDistance(efficientAxisProgressDistance: TEfficientSeqEjeElementsProgresiva): List[ProgresivaMilliseconds] = {
      val progresivaTimeStamp = fillDates(efficientAxisProgressDistance)
      val progresiva = progresivaTimeStamp.map(_.progresiva)
      val progresivaComplete = AlgorithmFill.completeProgresiva(progresiva)
      val minTime = progresivaTimeStamp.head.timeStamp
      val millis = ChronoUnit.MILLIS
      val times = progresivaTimeStamp.map(progresivaTime => (millis.between(minTime, progresivaTime.timeStamp), progresivaTime.timeStamp))
      progresivaComplete.zip(times).map {
        case (progresiva, (time, timeZoned)) => ProgresivaMilliseconds(progresiva, time, timeZoned)
      }
    }
  }

}

