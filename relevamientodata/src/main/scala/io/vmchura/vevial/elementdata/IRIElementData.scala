package io.vmchura.vevial.elementdata

import io.vmchura.vevial.EjeVialUtil.Coordinates
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point


case class IRIElementData(override val point: Option[UPoint],
                          speed: Option[UDouble],
                          vectorToNext: Option[UPlanarVector],
                          vectorToPrev: Option[UPlanarVector],
                          iriValue: Option[Double]) extends TElementWithPoint[IRIElementData] {
  override def withNextElement(a: IRIElementData): IRIElementData = copy(vectorToNext = a - this)

  override def withPrevElement(a: IRIElementData): IRIElementData = copy(vectorToPrev = this - a)

  private def - (o: IRIElementData): Option[UPlanarVector] = {
    for{
      up <- point
      uq <- o.point
      pv <- up.value -? uq.value
    }yield {
      UPlanarVector(pv,up.sigma2+uq.sigma2)
    }
  }
}

object IRIElementData{

  def apply(point: Option[UPoint], speed: Option[UDouble], vectorToNext: Option[UPlanarVector], vectorToPrev: Option[UPlanarVector], iriValue: Option[Double]): IRIElementData = new IRIElementData(point, speed, vectorToNext, vectorToPrev,iriValue)
  def apply(crudeIRIData: CrudeIRIData): IRIElementData = {
    import io.vmchura.vevial.EjeVialUtil.UtilFunctions.str2Double
    import io.vmchura.vevial.relevamiento.RelevamientoConfig.{sigma2DefaultGPSRoughmeterIII,sigma2DefaultSpeedRoughmeterIII}
    val point: Option[UPoint] = for{
      latitude <- str2Double(crudeIRIData.latitudeSTR)
      longitude <- str2Double(crudeIRIData.longitudeSTR)
    }yield{
      val coordinates = Coordinates(latitude,longitude).toUTMCoordinates()
      UPoint(Point(coordinates.Easting,coordinates.Northing),sigma2DefaultGPSRoughmeterIII)
    }
    val speed = str2Double(crudeIRIData.speedSTR).map{d => UDouble(d,sigma2DefaultSpeedRoughmeterIII)}

    val vectorToNext: Option[UPlanarVector] = None
    val vectorToPrev: Option[UPlanarVector] = None
    val iriValue: Option[Double] = str2Double(crudeIRIData.iriSTR)

    new IRIElementData(point, speed,  vectorToNext, vectorToPrev,iriValue)
  }


}