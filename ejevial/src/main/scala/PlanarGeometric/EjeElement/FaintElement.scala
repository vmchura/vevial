package PlanarGeometric.EjeElement

import PlanarGeometric.BasicGeometry.{ Point, PointUnitaryVector, TDirection}
import PlanarGeometric._

trait TFaintElement extends TSimpleEjeElement {
  def from: Point
  def end: Point
  override val length: Double = 0
  override val in: PointUnitaryVector = PointUnitaryVector(from,TDirection())
  override val out: PointUnitaryVector = PointUnitaryVector(end,TDirection())

  override def projectPoint(point: Point): Option[ElementPoint] = None

  override def pointIsInsideElement(point: Point): Boolean = false

  override def lengthToPoint(point: ElementPoint): Double = 0
}

case class FaintElement(from: Point, end: Point) extends TFaintElement
