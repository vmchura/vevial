package models
import io.vmchura.vevial.EjeVialBuilder.EfficientEjeByPoints
import io.vmchura.vevial.EjeVialUtil.{GeodesicCoordinates, Progresiva}
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva

import java.time.ZonedDateTime
case class ProgresivaMilliseconds(progresiva: Progresiva, millisFromStart: Long, timeZoned: ZonedDateTime)
case class ProgresivaTimeStamp(progresiva: Option[Progresiva], timeStamp: ZonedDateTime)
case class RawGeodesicTimeStamp(
    geodesicCoordinates: Option[GeodesicCoordinates],
    timeStamp: Option[ZonedDateTime]
)
case class GeodesicTimeStamp (
    geodesicCoordinatesOption: Option[GeodesicCoordinates],
    timeStamp: ZonedDateTime
) extends Ordered[GeodesicTimeStamp] {
  def toProgresivaTimeStamp(
      efficientEjeProgresiva: EfficientEjeProgresiva
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
  def toProgresivaTimeStamp(efficientEjeByPoints: EfficientEjeByPoints): ProgresivaTimeStamp = {
    val progresiva = for {
      geodesicCoordinates <- geodesicCoordinatesOption
      ep <- {
        val point = geodesicCoordinates.toPoint()
        efficientEjeByPoints.findProgresiva(point).map(_._2)
      }
    } yield {
      Progresiva(ep.toInt)
    }
    ProgresivaTimeStamp(progresiva, timeStamp)
  }

  override def compare(that: GeodesicTimeStamp): Int = timeStamp.compareTo(that.timeStamp)
}
