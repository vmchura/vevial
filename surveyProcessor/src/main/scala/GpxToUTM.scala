import io.vmchura.vevial.EjeVialUtil.Progresiva
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import models.{AlgorithmFill, GpxParser, ProgresivaMilliseconds, ProgresivaTimeStamp}

import java.time.temporal.ChronoUnit
import scala.xml.Node

object GpxToUTM {
  def parseFiles(gpxXML: Node, ejeEither: Either[Exception, EfficientEjeProgresiva]): Either[Exception, List[ProgresivaTimeStamp]] = {
    ejeEither.map{ eje =>
      GpxParser.parse(gpxXML).map(_.toProgressDistanceTimeStamp(eje))
    }
  }


  def parse(gpxXML: Node, ejeEither: Either[Exception, EfficientEjeProgresiva]): Either[Exception, List[ProgresivaMilliseconds]] = {
    val progresivasError = parseFiles(gpxXML, ejeEither)
    progresivasError.map(progresivaTimeStamp => {
      val progresiva = progresivaTimeStamp.map(_.progresiva)
      val progresivaComplete = AlgorithmFill.completeProgresiva(progresiva)
      val minTime = progresivaTimeStamp.head.timeStamp
      val millis = ChronoUnit.MILLIS
      val times = progresivaTimeStamp.map(progresivaTime => (millis.between(minTime, progresivaTime.timeStamp), progresivaTime.timeStamp))
      progresivaComplete.zip(times).map{
        case (progresiva, (time, timeZoned)) => ProgresivaMilliseconds(progresiva, time, timeZoned)
      }
    })
  }
}
