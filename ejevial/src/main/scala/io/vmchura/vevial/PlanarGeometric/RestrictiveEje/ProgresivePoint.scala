package io.vmchura.vevial.PlanarGeometric.RestrictiveEje

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, TPoint}
import io.vmchura.vevial.PlanarGeometric.ConfigParametersGeometric.areCloseInLinearReference


class ProgresivePoint(p: TPoint,val progresive: Double) extends  Point(p.x,p.y)  {
  def withNewInitialLinearReference(newProgresive: Double): ProgresivePoint = new ProgresivePoint(p,newProgresive)


  override def ==?(p: TPoint): Boolean = super.==?(p) && {
    p match {
      case pp: ProgresivePoint => areCloseInLinearReference(progresive,pp.progresive)
      case _ => false
    }
  }


}


