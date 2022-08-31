package models
import io.vmchura.vevial.EjeVialUtil.Coordinates

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import scala.xml.Node
object GpxParser {
  def parse(xml: Node): List[Option[RawGeodesicTimeStamp]] = {
    val puntualData = xml \\ "trkpt"
    puntualData.map{ node =>
      for{
        lat <- node.attribute("lat").flatMap(_.headOption).flatMap(_.text.toDoubleOption)
        lon <- node.attribute("lon").flatMap(_.headOption).flatMap(_.text.toDoubleOption)
      } yield{
        val zonedDateTime: Option[ZonedDateTime] = (node \ "time").headOption.map(node => ZonedDateTime.parse(node.text))
        RawGeodesicTimeStamp(Coordinates(lat, lon), zonedDateTime )
      }
    }.toList

  }
  def completeTimeStamp(initialData: List[Option[ZonedDateTime]]): List[ZonedDateTime] = {
    val unit = ChronoUnit.MILLIS
    val deltaMilliseconds = initialData.zip(initialData.tail).filter {
      case (Some(_), Some(_)) => true
      case _ => false
    }.map{
      case (Some(a), Some(b)) => unit.between(a,b)
      case _ => throw new IllegalStateException("it should had found something")
    }.minOption
    val dataArray = initialData.toArray
    val numberElements = initialData.length
    val offsetToAdd = Array.fill(numberElements)(Option.empty[Int])
    offsetToAdd.indices.foreach{ i =>
      if(i+1 < numberElements){
        (offsetToAdd(i+1), dataArray(i+1), offsetToAdd(i), dataArray(i)) match {
          case (_, Some(_), _, _) => ()
          case (Some(_), None, _, _) => ()
          case (None, None, _, Some(_)) => offsetToAdd(i+1) = Some(1)
          case (None, None, Some(current), None) => offsetToAdd(i+1) = Some(current+1)
          case (None, None, None, None) => ()
        }
      }
    }
    offsetToAdd.indices.reverse.foreach { i =>
      if (i - 1 >= 0) {
        (offsetToAdd(i - 1), dataArray(i - 1), offsetToAdd(i), dataArray(i)) match {
          case (_, Some(_), _, _) => ()
          case (Some(_), None, _, _) => ()
          case (None, None, _, Some(_)) => offsetToAdd(i - 1) = Some(-1)
          case (None, None, Some(current), None) => offsetToAdd(i - 1) = Some(current - 1)
          case (None, None, None, None) => ()
        }
      }
    }

    dataArray.zip(offsetToAdd).zipWithIndex.foreach{
      case ((Some(_), _), _) => ()
      case ((None, None), _) => ()
      case ((None, Some(delta)), i) => dataArray(i) = dataArray(i-delta).flatMap(other => deltaMilliseconds.map{ deltaLongMillis =>
        if(delta > 0){
          other.plus(deltaLongMillis*delta, unit)
        }else{
          other.minus(-deltaLongMillis*delta, unit)
        }

      })
    }
    if(dataArray.forall(_.isDefined)){
      dataArray.flatten.toList
    }else{
      throw new IllegalArgumentException("Impossible to complete deltas")
    }
  }

}
