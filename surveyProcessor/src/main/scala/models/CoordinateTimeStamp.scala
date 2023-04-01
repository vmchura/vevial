package models
import io.vmchura.vevial.EjeVialUtil.{GeodesicCoordinates, Progresiva}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.{EfficientEjeProgresiva, TEfficientSeqEjeElementsProgresiva}

import java.time.ZonedDateTime
case class ProgresivaMilliseconds(progresiva: Progresiva, millisFromStart: Long, timeZoned: ZonedDateTime)
case class ProgresivaTimeStamp(progresiva: Option[Progresiva], timeStamp: ZonedDateTime)
trait ProgressDistanceTimeStampAble[A <: ProgressDistanceTimeStampAble[A]] extends Ordered[A]{ this: A =>
  def timeStamp: ZonedDateTime
  def toProgressDistanceTimeStamp(efficientAxisProgressDistance: TEfficientSeqEjeElementsProgresiva): ProgresivaTimeStamp

  override def compare(that: A): Int = timeStamp.compareTo(that.timeStamp)
}

case class PointTimeStamp(point: Option[TPoint],
                          timeStamp: ZonedDateTime) extends ProgressDistanceTimeStampAble[PointTimeStamp] {
  override def toProgressDistanceTimeStamp(efficientAxisProgressDistance: TEfficientSeqEjeElementsProgresiva): ProgresivaTimeStamp = {
    val progressDistance = for{
      p <- point
      ep <- efficientAxisProgressDistance.projectPoint(p)
    }yield{
      Progresiva(efficientAxisProgressDistance.calcProgresive(ep).toInt)
    }
    ProgresivaTimeStamp(progressDistance, timeStamp)
  }
}

case class RawGeodesicTimeStamp(
    geodesicCoordinates: Option[GeodesicCoordinates],
    timeStamp: Option[ZonedDateTime]
)
case class GeodesicTimeStamp (
    geodesicCoordinatesOption: Option[GeodesicCoordinates],
    timeStamp: ZonedDateTime
) extends ProgressDistanceTimeStampAble[GeodesicTimeStamp] {
  def toProgressDistanceTimeStamp(
      efficientEjeProgresiva: TEfficientSeqEjeElementsProgresiva
  ): ProgresivaTimeStamp = {
    val progresiva = for {
      geodesicCoordinates <- geodesicCoordinatesOption
      ep <- {
        val point = geodesicCoordinates.toPoint()
        efficientEjeProgresiva.projectPoint(point)
      }
    }yield{
      Progresiva(efficientEjeProgresiva.calcProgresive(ep).toInt)
    }
    ProgresivaTimeStamp(progresiva, timeStamp)
  }
}


