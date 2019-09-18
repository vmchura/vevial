package PlanarGeometric.RestrictiveEje

import PlanarGeometric.BasicGeometry.{Point, TPoint}
import PlanarGeometric.ConfigParametersGeometric.areCloseInLinearReference


class ProgresivePoint(p: Point,val progresive: Double) extends  Point(p.x,p.y)  {
  def withNewInitialLinearReference(newProgresive: Double): ProgresivePoint = new ProgresivePoint(p,newProgresive)


  override def ==?(p: TPoint): Boolean = super.==?(p) && {
    p match {
      case pp: ProgresivePoint => areCloseInLinearReference(progresive,pp.progresive)
      case _ => false
    }
  }


}


