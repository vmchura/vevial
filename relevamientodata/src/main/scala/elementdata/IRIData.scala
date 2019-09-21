package elementdata

import EjeVialUtil.Coordinates
import PlanarGeometric.BasicGeometry.{PlanarVector, Point, TDirection}

trait TCrudeIRIData{
  def sectionID: String
  def subdistanceStr: String
  def totalDistanceStr: String
  def iriSTR: String
  def speedSTR: String
  def latitudeSTR: String
  def longitudeSTR: String
  def altituteSTR: String
  def eventsSTR: String
}

class CrudeIRIData(lineData: String) extends TCrudeIRIData{
  private val cols = lineData.split(",").map(_.trim)
  private val colsFilled = cols ++ Array.fill(9-cols.length)("")
  private val Array(secID, subDist, totalDist, iri, speed, latitudStr, longitudStr, altitudStr, eventsStr) = colsFilled

  override def sectionID: String = secID

  override def subdistanceStr: String = subDist

  override def totalDistanceStr: String = totalDist

  override def iriSTR: String = iri

  override def speedSTR: String = speed

  override def latitudeSTR: String = latitudStr

  override def longitudeSTR: String = longitudStr

  override def altituteSTR: String = altitudStr

  override def eventsSTR: String = eventsStr
}


case class IRIElementData(point: Option[UPoint],
                          speed: Option[UDouble],
                          vectorToNext: Option[UPlanarVector],
                          vectorToPrev: Option[UPlanarVector],
                          distanceToPrev: Option[UDouble]) extends TElementData[IRIElementData] {
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
  def apply(point: Option[UPoint],
            speed: Option[UDouble],
            direction: UDirection,
            distanceToNext: Option[UDouble], nextElement: Option[IRIElementData], prevElement: Option[IRIElementData]): IRIElementData = new IRIElementData(point, speed, direction, distanceToNext, nextElement, prevElement)
  def apply(crudeIRIData: CrudeIRIData): IRIElementData = {
    import EjeVialUtil.UtilFunctions.str2Double
    import relevamiento.RelevamientoConfig.{sigma2DefaultGPSRoughmeterIII,sigma2DefaultSpeedRoughmeterIII}
    val point: Option[UPoint] = for{
      latitude <- str2Double(crudeIRIData.latitudeSTR)
      longitude <- str2Double(crudeIRIData.longitudeSTR)
    }yield{
      val coordinates = Coordinates(latitude,longitude).toUTMCoordinates()
      UPoint(Point(coordinates.Easting,coordinates.Northing),sigma2DefaultGPSRoughmeterIII)
    }
    val speed = str2Double(crudeIRIData.speedSTR).map{d => UDouble(d,sigma2DefaultSpeedRoughmeterIII)}
    val direction = UDirection(TDirection(),Double.PositiveInfinity)
    val distanceToNext: Option[UDouble] = None
    val nextElement: Option[IRIElementData] = None
    val prevElement: Option[IRIElementData] = None
    new IRIElementData(point, speed, direction, distanceToNext, nextElement, prevElement)
  }


}