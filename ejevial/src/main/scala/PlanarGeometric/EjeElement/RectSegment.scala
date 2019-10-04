package PlanarGeometric.EjeElement

import PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector}
import PlanarGeometric.ConfigParametersGeometric.areCloseInLinearReference
trait TRectSegment extends TSimpleEjeElement {
  val originPoint: Point
  val endPoint: Point
  private val pVector = endPoint-originPoint
  override val length: Double = !pVector
  override val in: PointUnitaryVector = PointUnitaryVector(originPoint,pVector.direction)
  override val out: PointUnitaryVector = PointUnitaryVector(endPoint,pVector.direction)

  override def projectPoint(point: Point): Option[ElementPoint] = {
    val v = point - originPoint
    val s = in.direction
    val distance  =  (v * s) / (s * s)
    val projection = originPoint + s * distance
    if (pointIsInsideElement(projection))
      Some(ElementPoint(projection,point-?projection,this))
    else
      None
  }

  override def pointIsInsideElement(point: Point): Boolean = areCloseInLinearReference(length, !(point - originPoint) + !(point - endPoint))

  override def lengthToPoint(point: ElementPoint): Double = !(point-originPoint)

  private val fe = Seq(originPoint,endPoint)

  override val leftmostPoint: Point = fe.minBy(_.x)

  override val rightmostPoint: Point = fe.maxBy(_.x)

  override val upperPoint: Point = fe.maxBy(_.y)

  override val lowerPoint: Point = fe.minBy(_.y)
}

case class RectSegment(originPoint: Point, endPoint: Point) extends TRectSegment{

  require((endPoint-?originPoint).isDefined)


}

