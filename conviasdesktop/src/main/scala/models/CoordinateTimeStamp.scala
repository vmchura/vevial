package models
import io.vmchura.vevial.EjeVialUtil.{Coordinates, GeodesicCoordinates, Progresiva, UTMCoordinates}
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva

import java.time.ZonedDateTime
case class ProgresivaTimeStamp (progresiva: Progresiva, timeStamp: ZonedDateTime)
case class RawGeodesicTimeStamp (geodesicCoordinates: GeodesicCoordinates, timeStamp: Option[ZonedDateTime])
case class GeodesicTimeStamp (geodesicCoordinates: GeodesicCoordinates, timeStamp: ZonedDateTime){
  def toProgresivaTimeStamp(efficientEjeProgresiva: EfficientEjeProgresiva): Option[ProgresivaTimeStamp] = {
    val point = geodesicCoordinates.toPoint()
    val ep = efficientEjeProgresiva.projectPoint(point)
    ep.map{
      ep => ProgresivaTimeStamp(Progresiva(efficientEjeProgresiva.calcProgresive(ep).toInt), timeStamp)
    }
  }
}
