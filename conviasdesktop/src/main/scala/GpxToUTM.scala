import io.vmchura.vevial.EjeVialBuilder.LandXMLToEje
import io.vmchura.vevial.EjeVialUtil.Progresiva
import models.{AlgorithmFill, GpxParser, ProgresivaMilliseconds, ProgresivaTimeStamp}

import java.time.temporal.ChronoUnit
import scala.reflect.io.File
import scala.io.Codec
import scala.xml.Node

object GpxToUTM {
  def parseFiles(gpxXML: Node, ejeXMLFile: File): Either[Exception, List[ProgresivaTimeStamp]] = {
    val ejeEither = new LandXMLToEje(ejeXMLFile.reader(Codec("UTF-8"))).toEje
    ejeEither.map{ eje =>
      GpxParser.parse(gpxXML).map(_.toProgresivaTimeStamp(eje))
    }
  }
  def completeProgresiva(initialData: List[Option[Progresiva]]): List[Progresiva] =
    AlgorithmFill.completeTimeStamp[Progresiva, Int](initialData,
      lista => {
        val suma = lista.map{case (a,b) => (a.progresiva-b.progresiva).abs}.sum
        Some(suma/lista.length)
      }, (other, deltaLongMillis, delta) =>
        Progresiva(other.progresiva + deltaLongMillis * delta))

  def parse(gpxXML: Node, ejeXMLFile: File): Either[Exception, List[ProgresivaMilliseconds]] = {
    val progresivasError = parseFiles(gpxXML, ejeXMLFile)
    progresivasError.map(progresivaTimeStamp => {
      val progresiva = progresivaTimeStamp.map(_.progresiva)
      val progresivaComplete = completeProgresiva(progresiva)
      val minTime = progresivaTimeStamp.head.timeStamp
      val millis = ChronoUnit.MILLIS
      val times = progresivaTimeStamp.map(progresivaTime => millis.between(minTime, progresivaTime.timeStamp))
      progresivaComplete.zip(times).map{
        case (progresiva, time) => ProgresivaMilliseconds(progresiva, time)
      }
    })
  }
}
