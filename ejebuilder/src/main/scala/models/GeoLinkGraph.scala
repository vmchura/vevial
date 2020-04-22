package models

import AutomaticBuilder.models.{ActionImproveEje, ProjectionOverElement, TElementCanImprove, TProjection}
import algorithms.LinearEquationsSolver
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{PointUnitaryVector, TPoint}

class GeoLinkGraph(val in: PointUnitaryVector,val out: PointUnitaryVector,
                   var prev: Option[TLinkPoint]=None, var next: Option[TLinkPoint]=None
                  ) extends TLinkPoint with TElementCanImprove{

  override val elements: Seq[TEjeElementTemporal] = {
    LinearEquationsSolver.buildCircleTangent(in,out) match {
      case Some(x) => {
        val c = CircleTemporal(x.originPoint,x.centerPoint,x.endPoint,x.antiClockWise,this)
        val left = if(c.originPoint ==? in.point) None else Some(RectTemporal(in.point,c.originPoint,this))
        val right = if(c.endPoint ==? out.point) None else Some(RectTemporal(c.endPoint,out.point,this))
        List(left,Some(c),right).flatten
      }
      case None => {

        val recta = RectTemporal(in.point,out.point,this)
        val f0 = FaintTemporal(in.point,recta.in.point,this)
        val f1 = FaintTemporal(recta.out.point,out.point,this)
        List(f0,recta,f1)
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
