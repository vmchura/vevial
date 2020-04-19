package io.vmchura.vevial.PlanarGeometric.EjeElement

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TDirection, TPoint}
import io.vmchura.vevial.PlanarGeometric._

trait TFaintElement extends TSimpleEjeElement {
  def from: TPoint
  def end: TPoint
  override val length: Double = 0
  override val in: PointUnitaryVector = PointUnitaryVector(from,TDirection())
  override val out: PointUnitaryVector = PointUnitaryVector(end,TDirection())

  override def projectPoint(point: TPoint): Option[ElementPoint] = None

  override def pointIsInsideElement(point: TPoint): Boolean = false

  override def lengthToPoint(point: ElementPoint): Double = 0

  private val fe = Seq(from,end)
  override lazy val leftmostPoint: TPoint = fe.minBy(_.x)

  override lazy val rightmostPoint: TPoint = fe.maxBy(_.x)

  override lazy val upperPoint: TPoint = fe.maxBy(_.y)

  override lazy val lowerPoint: TPoint = fe.minBy(_.y)
}

case class FaintElement(from: TPoint, end: TPoint) extends TFaintElement
