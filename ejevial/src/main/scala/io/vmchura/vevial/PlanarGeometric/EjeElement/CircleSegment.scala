package io.vmchura.vevial.PlanarGeometric.EjeElement

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TDirection, TPoint}
import io.vmchura.vevial.PlanarGeometric.ConfigParametersGeometric.areCloseInLinearReference

trait TCircleSegment extends TSimpleEjeElement{
  def originPoint: TPoint
  def centerPoint: TPoint
  def endPoint: TPoint
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

  override def projectPoint(point: TPoint): Option[ElementPoint] = {(point -? centerPoint).flatMap { v =>
    val pc = centerPoint + (v.direction * radius)
    if (pointIsInsideElement(pc)) {
      Some(ElementPoint(pc,point-?pc,this))
    }
    else {
      None
    }
  }}

  override def pointIsInsideElement(point: TPoint): Boolean = {
    if((point -? in.point).isEmpty || (point -? out.point).isEmpty){
      true
    }else {
      val arc1 = CircleSegment(originPoint, centerPoint, point, antiClockWise)
      val arc2 = CircleSegment(point, centerPoint, endPoint, antiClockWise)

      areCloseInLinearReference(arc1.length + arc2.length, length)

    }
  }

  override def lengthToPoint(point: ElementPoint): Double = {
    if((point -? in.point).isEmpty || (point -? out.point).isEmpty){
      if((point -? in.point).isEmpty){
        0.0
      }else{
        length
      }
    }else {
      val v = point - centerPoint
      val u = originPoint - centerPoint
      val beta = if(antiClockWise)  u \/ v else v \/u
      beta * radius
    }
  }
  override val in: PointUnitaryVector =  PointUnitaryVector(originPoint,(originPoint-centerPoint).direction <¬ (if (antiClockWise) 1 else 3 ))
  override val out: PointUnitaryVector = PointUnitaryVector(endPoint,(endPoint-centerPoint).direction <¬ (if (antiClockWise) 1 else 3 ))


  override lazy val leftmostPoint: TPoint = centerPoint + (TDirection(-1,0)*radius)


  override lazy val rightmostPoint: TPoint = centerPoint + (TDirection(1,0)*radius)

  override lazy val upperPoint: TPoint = centerPoint + (TDirection(0,1)*radius)


  override lazy val lowerPoint: TPoint = centerPoint + (TDirection(0,-1)*radius)
}

case class CircleSegment(originPoint: TPoint, centerPoint: TPoint, endPoint: TPoint, antiClockWise: Boolean) extends  TCircleSegment {
  require((originPoint-?centerPoint).isDefined)
  require((endPoint-?centerPoint).isDefined)
  require(areCloseInLinearReference(!(originPoint-centerPoint),!(endPoint-centerPoint)))


}
