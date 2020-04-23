package io.vmchura.vevial.PlanarGeometric.EjeElement

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{PlanarVector, Point, TPoint}

case class ElementPoint(point: TPoint, toSource: Option[PlanarVector], ejeElementOwner: TEjeElement) extends TPoint {
  override val x: Double = point.x
  override val y: Double = point.y
  //def changeOwner[B <: TEjeElement[B]](newOwner: B): ElementPoint[B] = ElementPoint(point,toSource,newOwner)
  val sourcePoint: TPoint = toSource.map(s => point +s ).getOrElse(point)
}
