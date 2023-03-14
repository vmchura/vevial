package io.vmchura.vevial.elementdata

import io.vmchura.vevial.EjeVialUtil.Coordinates
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point

import java.time.ZonedDateTime
import scala.xml.Node

case class GPXElementData(override val point: Option[UPoint],
                          speed: Option[UDouble],
                          vectorToNext: Option[UPlanarVector],
                          vectorToPrev: Option[UPlanarVector],
                          zonedTime: Option[ZonedDateTime],
                          originalData: Node) extends TElementWithPoint[GPXElementData] {
  override def withNextElement(a: GPXElementData): GPXElementData = copy(vectorToNext = a - this)

  override def withPrevElement(a: GPXElementData): GPXElementData = copy(vectorToNext = a - this)

  private def -(o: GPXElementData): Option[UPlanarVector] = {
    for {
      up <- point
      uq <- o.point
      pv <- up.value -? uq.value
    } yield {
      UPlanarVector(pv, up.sigma2 + uq.sigma2)
    }
  }
}

object GPXElementData {
  import io.vmchura.vevial.relevamiento.RelevamientoConfig.sigma2DefaultGPSGoPro7
  def apply(node: Node): GPXElementData = {

    val zonedDateTime = (node \ "time").headOption.map(node => ZonedDateTime.parse(node.text))
    val point: Option[UPoint] = for {
      latitude <- node.attribute("lat").flatMap(_.headOption).flatMap(_.text.toDoubleOption)
      longitude <- node.attribute("lon").flatMap(_.headOption).flatMap(_.text.toDoubleOption)
    } yield {
      val coordinates = Coordinates(latitude, longitude).toUTMCoordinates()
      UPoint(Point(coordinates.Easting, coordinates.Northing), sigma2DefaultGPSGoPro7)
    }
    val speed = Option.empty[UDouble]
    val vectorToNext: Option[UPlanarVector] = None
    val vectorToPrev: Option[UPlanarVector] = None

    new GPXElementData(point, speed, vectorToNext, vectorToPrev, zonedDateTime, node)
  }
}
