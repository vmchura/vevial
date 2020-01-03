package io.vmchura.vevial.Calculator.models.singularitypoint
import java.util.UUID

import io.vmchura.vevial.EjeVialUtil.{Coordinates, GeodesicCoordinates}
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import SingularityPointRaw.toDouble
sealed trait SingularityPointRaw {
  def description: String
  def id: String
  def progresivaIni: Option[Int]
  def progresivaFin: Option[Int]
  def delta: Int
  def order: Int
  final def toSingularityPoint(groupID: UUID): Option[SingularityPoint] = {


    for{
      id <- toDouble(id).map(_.toInt)
      progIni <- progresivaIni
      progFin <- progresivaFin
    }yield{
      SingularityPoint(groupID,id,description,progIni,progFin,delta,order)
    }


  }




}


object SingularityPointRaw{

  case class ProgresivePointRaw(order: Int, id: String, description: String, progresivaIni: Option[Int], delta: Int = 0) extends SingularityPointRaw {
    override def progresivaFin: Option[Int] = progresivaIni
  }

  case class ProgresivaRangeRaw(order: Int, id: String, description: String, progresivaIni: Option[Int], progresivaFin: Option[Int], delta: Int = 0) extends SingularityPointRaw

  case class LatLongPointRaw(order: Int, id: String, description: String, coordinatesIni: GeodesicCoordinates, ejeVial: EfficientEjeProgresiva, delta: Int = 0) extends SingularityPointRaw {
    private val point = coordinatesIni.toUTMCoordinates().toPoint()
    private val elementPoint = ejeVial.projectPoint(point)

    override def progresivaIni: Option[Int] = elementPoint.map(ep => ejeVial.calcProgresive(ep).toInt)

    override def progresivaFin: Option[Int] = progresivaIni
  }

  case class LatLongRangeRaw(order: Int, id: String, description: String, coordinatesIni: GeodesicCoordinates, coordinatesFin: GeodesicCoordinates, ejeVial: EfficientEjeProgresiva, delta: Int = 0) extends SingularityPointRaw {

    override def progresivaIni: Option[Int] = ejeVial.projectPoint(coordinatesIni.toUTMCoordinates().toPoint()).map(ep => ejeVial.calcProgresive(ep).toInt)

    override def progresivaFin: Option[Int] = ejeVial.projectPoint(coordinatesFin.toUTMCoordinates().toPoint()).map(ep => ejeVial.calcProgresive(ep).toInt)
  }

  case class UnknowSingularityRaw(order: Int, id: String) extends SingularityPointRaw {
    override def description: String = ""

    override def progresivaIni: Option[Int] = None

    override def progresivaFin: Option[Int] = None

    override def delta: Int = 0
  }

  def toDouble(str: String): Option[Double] = try{Some(str.toDouble)} catch {case _: Throwable => None}
  def apply(order: Int, raw: ParsedRawString, ejeVial: EfficientEjeProgresiva): SingularityPointRaw = {
    val defaultValue = UnknowSingularityRaw(order,raw.stringToSave)

    def toDoubleByIndex(indx: Int): Option[Double] = if(raw.originalValues.length>indx) toDouble(raw.originalValues(indx)) else None
    toDoubleByIndex(3).map(_.toInt).map{ delta =>
      val parametersDouble = (4 to 9).map(toDoubleByIndex)
      val parametersInt = parametersDouble.map(_.map(_.toInt))

      val id = raw.originalValues(0)
      val desc = raw.originalValues(1)
      raw.originalValues.length match {
        case l if l==5 && raw.originalValues(2).toLowerCase().equals("p") => ProgresivePointRaw(order, id,desc,parametersInt(0),delta)
        case l if l==6 && raw.originalValues(2).toLowerCase().equals("p") => ProgresivaRangeRaw(order, id,desc,parametersInt(0),parametersInt(1),delta)
        case l if l==6 && raw.originalValues(2).toLowerCase().equals("l") =>
          (for{
            lat <- parametersDouble(0)
            lon <- parametersDouble(1)
          }yield{
            val coordinates = Coordinates(lat,lon)
            LatLongPointRaw(order, id,desc,coordinates,ejeVial,delta)
          }).getOrElse(defaultValue)
        case l if l==8 && raw.originalValues(2).toLowerCase().equals("l") =>
          (for{
            lat0 <- parametersDouble(0)
            lon0 <- parametersDouble(1)
            lat1 <- parametersDouble(2)
            lon1 <- parametersDouble(3)
          }yield{
            val coordinates0 = Coordinates(lat0,lon0)
            val coordinates1 = Coordinates(lat1,lon1)
            LatLongRangeRaw(order,id,desc,coordinates0,coordinates1,ejeVial,delta)
          }).getOrElse(defaultValue)
        case _ => defaultValue
      }
    }.getOrElse(defaultValue)


  }
}



