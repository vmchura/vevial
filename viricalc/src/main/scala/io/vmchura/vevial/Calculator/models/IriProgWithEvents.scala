package io.vmchura.vevial.Calculator.models

import io.vmchura.vevial.EjeVialUtil.Coordinates
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point
import io.vmchura.vevial.elementdata.{CrudeIRIData, DataWithPoint, TElementData, TElementWithPoint, UDouble, UPlanarVector, UPoint}

case class IriProgWithEvents(override val point: Option[UPoint],iriValue: Option[Double], event: String)
  extends TElementWithPoint[IriProgWithEvents]  {
  override def speed: Option[UDouble] = None

  override def vectorToNext: Option[UPlanarVector] = None

  override def vectorToPrev: Option[UPlanarVector] = None

  override def withNextElement(a: IriProgWithEvents): IriProgWithEvents = this

  override def withPrevElement(a: IriProgWithEvents): IriProgWithEvents = this
}
object IriProgWithEvents{
  def apply(crudeIRIData: CrudeIRIData): IriProgWithEvents = {
    import io.vmchura.vevial.EjeVialUtil.UtilFunctions.str2Double
    import io.vmchura.vevial.relevamiento.RelevamientoConfig.{sigma2DefaultGPSRoughmeterIII,sigma2DefaultSpeedRoughmeterIII}
    val point: Option[UPoint] = for{
      latitude <- str2Double(crudeIRIData.latitudeSTR)
      longitude <- str2Double(crudeIRIData.longitudeSTR)
    }yield{
      val coordinates = Coordinates(latitude,longitude).toUTMCoordinates()
      UPoint(Point(coordinates.Easting,coordinates.Northing),sigma2DefaultGPSRoughmeterIII)
    }

    val iriValue: Option[Double] = str2Double(crudeIRIData.iriSTR)

    new IriProgWithEvents(point,iriValue, crudeIRIData.eventsSTR)
  }
}
