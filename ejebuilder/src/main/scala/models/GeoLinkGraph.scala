package models

import AutomaticBuilder.models.{ActionImproveEje, ProjectionOverElement, TElementCanImprove, TProjection}
import algorithms.LinearEquationsSolver
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TDirection.{AnyDirection, Direction}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{PointUnitaryVector, TPoint}

import scala.collection.mutable.ListBuffer

class GeoLinkGraph(val in: PointUnitaryVector,val out: PointUnitaryVector,
                   var prev: Option[TLinkPoint]=None, var next: Option[TLinkPoint]=None
                  ) extends TLinkPoint {
  private val pointsDataCoveringMutable = ListBuffer.empty[TPoint]
  def pointsDataCovering: Iterable[TPoint] = pointsDataCoveringMutable.toSeq
  def addPointCovered(tpoint: TPoint): Unit = pointsDataCoveringMutable += tpoint

  override val elements: Seq[TEjeElementTemporal] = {
    lazy val simpleRectElements = {
      val recta = RectTemporal(in.point,out.point,this)
      val f0 = FaintTemporal(in.point,recta.in.point,this)
      val f1 = FaintTemporal(recta.out.point,out.point,this)
      List(f0,recta,f1)
    }
    (in.direction,out.direction) match {
      case (AnyDirection(),AnyDirection()) => simpleRectElements
      case (AnyDirection(),_: Direction) => simpleRectElements
      case (_: Direction, AnyDirection()) =>simpleRectElements
      case (_: Direction,_:Direction) =>
        LinearEquationsSolver.buildCircleTangent(in,out) match {
          case Some(x) => {
            val c = CircleTemporal(x.originPoint,x.centerPoint,x.endPoint,x.antiClockWise,this)
            val left = if(c.originPoint ==? in.point) None else Some(RectTemporal(in.point,c.originPoint,this))
            val right = if(c.endPoint ==? out.point) None else Some(RectTemporal(c.endPoint,out.point,this))
            List(left,Some(c),right).flatten
          }
          case None => simpleRectElements
        }
    }

  }

  override val length: Double = elements.map(_.length).sum

  override def calcProjection(p: TPoint): Option[TProjection] = {
    elements.scanLeft(0d: Double,Option.empty[TProjection]){
      case ((acumLength,_),element: TEjeElementTemporal) =>
        (acumLength + element.length,element.projectionOverElement(p).map(tp => ProjectionOverElement(tp.distanceOverElement+acumLength, tp.distanceNormal)))
      case _ => throw new IllegalArgumentException("elements are not temporal")

    }.flatMap(_._2).minByOption(pr => Math.abs(pr.distanceNormal))

  }

}
