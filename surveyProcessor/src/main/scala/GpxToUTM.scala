import io.vmchura.vevial.EjeVialBuilder.EfficientEjeByPoints
import io.vmchura.vevial.EjeVialUtil.Progresiva
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import models.{AlgorithmFill, GpxParser, ProgresivaMilliseconds, ProgresivaTimeStamp}

import java.time.temporal.ChronoUnit
import scala.xml.Node

object GpxToUTM {
  def parseFiles(gpxXML: Node, ejeEither: Either[Exception, EfficientEjeProgresiva]): Either[Exception, List[ProgresivaTimeStamp]] = {
    ejeEither.map{ eje =>
      GpxParser.parse(gpxXML).map(_.toProgresivaTimeStamp(eje))
    }
  }

  def parseFilesByEjePoints(gpxXML: Node, eje:EfficientEjeByPoints): Either[Exception, List[ProgresivaTimeStamp]] = {

     Right(GpxParser.parse(gpxXML).map(_.toProgresivaTimeStamp(eje)))

  }
  def completeProgresiva(initialData: List[Option[Progresiva]]): List[Progresiva] =
    AlgorithmFill.completeTimeStamp[Progresiva, Int](initialData,
      lista => {
        val suma = lista.map{case (a,b) => (a.progresiva-b.progresiva).abs}.sum
        Some(suma/lista.length)
      }, (other, deltaLongMillis, delta) =>
        Progresiva(other.progresiva + deltaLongMillis * delta))

  def parse(gpxXML: Node, ejeEither: Either[Exception, EfficientEjeProgresiva]): Either[Exception, List[ProgresivaMilliseconds]] = {
    val progresivasError = parseFiles(gpxXML, ejeEither)
    progresivasError.map(progresivaTimeStamp => {
      val progresiva = progresivaTimeStamp.map(_.progresiva)
      val progresivaComplete = completeProgresiva(progresiva)
      val minTime = progresivaTimeStamp.head.timeStamp
      val millis = ChronoUnit.MILLIS
      val times = progresivaTimeStamp.map(progresivaTime => (millis.between(minTime, progresivaTime.timeStamp), progresivaTime.timeStamp))
      progresivaComplete.zip(times).map{
        case (progresiva, (time, timeZoned)) => ProgresivaMilliseconds(progresiva, time, timeZoned)
      }
    })
  }

  def parse(gpxXML: Node, eje: EfficientEjeByPoints): Either[Exception, List[ProgresivaMilliseconds]] = {
    val progresivasError = parseFilesByEjePoints(gpxXML, eje)
    progresivasError.map(progresivaTimeStamp => {
      val progresiva = progresivaTimeStamp.map(_.progresiva)
      val progresivaComplete = completeProgresiva(progresiva)
      val minTime = progresivaTimeStamp.head.timeStamp
      val millis = ChronoUnit.MILLIS
      val times = progresivaTimeStamp.map(progresivaTime => (millis.between(minTime, progresivaTime.timeStamp), progresivaTime.timeStamp))
      progresivaComplete.zip(times).map {
        case (progresiva, (time, timeZoned)) => ProgresivaMilliseconds(progresiva, time, timeZoned)
      }
    })
  }
}
