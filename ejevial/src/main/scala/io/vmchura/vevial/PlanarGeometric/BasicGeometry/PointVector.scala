package io.vmchura.vevial.PlanarGeometric.BasicGeometry

import io.vmchura.vevial.PlanarGeometric.ConfigParametersGeometric.areCloseInLinearReference
trait TPointVector {
  def point: Point
  def direction: TDirection
  def magnitude: Double
  def ==? (o: TPointVector): Boolean = point ==? o.point && direction==? o.direction && areCloseInLinearReference(magnitude,o.magnitude)
}
case class PointPlanarVector(point: Point, planarVector: PlanarVector) extends TPointVector {
  override val direction: TDirection = planarVector.direction
  override val magnitude: Double = planarVector.magnitude
}

case class PointUnitaryVector(point: Point, direction: TDirection) extends  TPointVector {
  override val magnitude: Double = direction.magnitude
}
