package PlanarGeometric.EjeElement

import PlanarGeometric.BasicGeometry.{PlanarVector, Point, TPoint}

case class ElementPoint(point: Point, toSource: Option[PlanarVector], ejeElementOwner: TEjeElement) extends TPoint {
  override val x: Double = point.x
  override val y: Double = point.y
  //def changeOwner[B <: TEjeElement[B]](newOwner: B): ElementPoint[B] = ElementPoint(point,toSource,newOwner)
}
