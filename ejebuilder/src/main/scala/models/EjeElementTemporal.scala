package models

import AutomaticBuilder.models.{ProjectionOverElement, TProjection}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, TDirection, TPoint}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{TCircleSegment, TEjeElement, TFaintElement, TRectSegment, TSimpleEjeElement}


sealed trait TEjeElementTemporal extends TSimpleEjeElement{
  def ejeSection: TLinkPoint
  final def projectionOverElement(point: TPoint): Option[TProjection] = {
    projectPoint(point).map{ ep =>
      val lengthOverElement = lengthToPoint(ep)
      val normalToPoint = ep.toSource.map{ pv =>
        (tangent(ep) x pv.direction).z.sign * pv.magnitude
      }.getOrElse(0d)
      ProjectionOverElement(lengthOverElement,normalToPoint)
    }
  }
  final def pointFromProjection(tprojection: TProjection): Option[Point] = {
    pointOnElement(tprojection.distanceOverElement).map{ q =>
      q + ((tangent(q) <¬ (1))*tprojection.distanceNormal)

    }
  }

  def pointOnElement(length: Double): Option[TPoint]
  def tangent(insidePoint: TPoint): TDirection
}

case class FaintTemporal(from: TPoint, end: TPoint, ejeSection: TLinkPoint) extends TFaintElement with TEjeElementTemporal {


  override def tangent(insidePoint: TPoint): TDirection = TDirection()

  override def pointOnElement(length: Double): Option[TPoint] = None
}
case class RectTemporal(originPoint: TPoint, endPoint: TPoint, ejeSection: TLinkPoint) extends TRectSegment with TEjeElementTemporal {

  override def tangent(insidePoint: TPoint): TDirection = in.direction

  override def pointOnElement(lengthParam: Double): Option[TPoint] = if(lengthParam < 0 || lengthParam > length) None else Some(in.point  + (in.direction*lengthParam))
}
case class CircleTemporal(originPoint: TPoint, centerPoint: TPoint, endPoint: TPoint, antiClockWise: Boolean, ejeSection: TLinkPoint) extends TCircleSegment with TEjeElementTemporal {

  override def tangent(insidePoint: TPoint): TDirection = (insidePoint - centerPoint).direction <¬ (if(antiClockWise) 1 else 3)

  override def pointOnElement(lengthParam: Double): Option[TPoint] = if(lengthParam < 0 || lengthParam > length) None else {
    val v = in.point - centerPoint
    val alpha = lengthParam/radius
    Some(centerPoint +  (v << (alpha * (if(antiClockWise) 1 else -1))))

  }
}