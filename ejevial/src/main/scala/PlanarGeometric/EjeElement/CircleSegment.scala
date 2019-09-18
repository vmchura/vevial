package PlanarGeometric.EjeElement

import PlanarGeometric.BasicGeometry.{ Point, PointUnitaryVector, TDirection}
import PlanarGeometric.ConfigParametersGeometric.areCloseInLinearReference
import PlanarGeometric._
trait TCircleSegment extends TSimpleEjeElement{
  def originPoint: Point
  def centerPoint: Point
  def endPoint: Point
  def antiClockWise: Boolean
  def radius: Double = !(originPoint - centerPoint)

  /**
    * angle alpha in radians
    * alpha is positive if  is anticlockWise
    * alpha is negative if  is clockwise
    */
  def alpha: Double = {
    val angle = (originPoint-centerPoint) \/ (endPoint-centerPoint)
    if(antiClockWise)
      angle
    else
      angle-2*Math.PI
  }
  /**
    * initialAngle is the angle from X+ axis to (center->originPoint) in radians
    *
    */
  def initialAngle: Double = TDirection(1,0) \/ (originPoint-centerPoint)
  override def length: Double = Math.abs(alpha*radius)

  override def projectPoint(point: Point): Option[ElementPoint] = {(point -? centerPoint).flatMap { v =>
    val pc = centerPoint + (v.direction * radius)
    if (pointIsInsideElement(pc)) {
      Some(ElementPoint(pc,point-?pc,this))
    }
    else {
      None
    }
  }}

  override def pointIsInsideElement(point: Point): Boolean = {
    val arc1 = CircleSegment(originPoint, centerPoint, point, antiClockWise)
    val arc2 = CircleSegment(point, centerPoint, endPoint, antiClockWise)

    areCloseInLinearReference(arc1.length + arc2.length, length)
  }

  override def lengthToPoint(point: ElementPoint): Double = {
    val v = point - centerPoint
    val u = originPoint- centerPoint
    val beta = u \/ v
    beta*radius
  }
  override val in: PointUnitaryVector =  PointUnitaryVector(originPoint,(originPoint-centerPoint).direction <¬ (if (antiClockWise) 1 else 3 ))
  override val out: PointUnitaryVector = PointUnitaryVector(endPoint,(endPoint-centerPoint).direction <¬ (if (antiClockWise) 1 else 3 ))

}

case class CircleSegment(originPoint: Point, centerPoint: Point, endPoint: Point, antiClockWise: Boolean) extends  TCircleSegment {
  require((originPoint-?centerPoint).isDefined)
  require((endPoint-?centerPoint).isDefined)
  require(areCloseInLinearReference(!(originPoint-centerPoint),!(endPoint-centerPoint)))
}
