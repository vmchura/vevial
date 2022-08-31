package models
import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import scala.xml.{Node, XML}
object GpxParser {
  def parse(xmlFile: Node): List[RawGeodesicTimeStamp] = ???
  def completeTimeStamp(initialData: List[Option[ZonedDateTime]]): List[ZonedDateTime] = {
    val unit = ChronoUnit.MILLIS
    val deltaMilliseconds = initialData.zip(initialData.tail).find {
      case (Some(_), Some(_)) => true
      case _ => false
    }.map{
      case (Some(a), Some(b)) => unit.between(a,b)
      case _ => throw new IllegalStateException("it should had found something")
    }
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

    println(deltaMilliseconds)
    println(offsetToAdd.mkString("-"))
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
