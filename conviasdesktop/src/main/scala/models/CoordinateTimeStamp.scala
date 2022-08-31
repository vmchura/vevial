package models
import io.vmchura.vevial.EjeVialUtil.{
  Coordinates,
  GeodesicCoordinates,
  Progresiva,
  UTMCoordinates
}
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva

import java.time.ZonedDateTime
case class ProgresivaMilliseconds(progresiva: Progresiva, millisFromStart: Long)
case class ProgresivaTimeStamp(progresiva: Option[Progresiva], timeStamp: ZonedDateTime)
case class RawGeodesicTimeStamp(
    geodesicCoordinates: Option[GeodesicCoordinates],
    timeStamp: Option[ZonedDateTime]
)
case class GeodesicTimeStamp(
    geodesicCoordinatesOption: Option[GeodesicCoordinates],
    timeStamp: ZonedDateTime
) {
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
}
