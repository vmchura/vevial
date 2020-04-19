package io.vmchura.vevial.PlanarGeometric.EjeElement

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TPoint}
import io.vmchura.vevial.PlanarGeometric.ConfigParametersGeometric.areCloseInLinearReference
trait TRectSegment extends TSimpleEjeElement {
  val originPoint: TPoint
  val endPoint: TPoint
  private val pVector = endPoint-originPoint
  override val length: Double = !pVector
  override val in: PointUnitaryVector = PointUnitaryVector(originPoint,pVector.direction)
  override val out: PointUnitaryVector = PointUnitaryVector(endPoint,pVector.direction)

  override def projectPoint(point: TPoint): Option[ElementPoint] = {
    val v = point - originPoint
    val s = in.direction
    val distance  =  (v * s) / (s * s)
    val projection = originPoint + s * distance
    if (pointIsInsideElement(projection))
      Some(ElementPoint(projection,point-?projection,this))
    else
      None
  }

  override def pointIsInsideElement(point: TPoint): Boolean = areCloseInLinearReference(length, !(point - originPoint) + !(point - endPoint))

  override def lengthToPoint(point: ElementPoint): Double = !(point-originPoint)

  private val fe = Seq(originPoint,endPoint)

  override val leftmostPoint: TPoint = fe.minBy(_.x)

  override val rightmostPoint: TPoint = fe.maxBy(_.x)

  override val upperPoint: TPoint = fe.maxBy(_.y)

  override val lowerPoint: TPoint = fe.minBy(_.y)
}

case class RectSegment(originPoint: TPoint, endPoint: TPoint) extends TRectSegment{

  require((endPoint-?originPoint).isDefined)


}

