package io.vmchura.vevial.PlanarGeometric.BasicEje

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point
import io.vmchura.vevial.PlanarGeometric.EjeElement.{CircleSegment, ElementPoint, RectSegment, TCircleSegment, TEjeElement, TFaintElement, TRectSegment}

trait EjeElement2PointsGenerator[A <: TEjeElement]{
  def generatePoints(a: A): List[ElementPoint]
}

object PointsOnElementGenerator {

  //(implicit conv: EjeElement2PointsGenerator[A])



  implicit val ejePointGenerator: EjeElement2PointsGenerator[TEjeElement] = new EjeElement2PointsGenerator[TEjeElement] {
    def calcPointToDistance[U <: TEjeElement](e: U, distance: Double): ElementPoint = {
      val p: Point = e match {
        case r: TRectSegment => r.originPoint+(r.in.direction*distance.toDouble)
        case c: TCircleSegment =>
          val beta = distance/c.radius
          val u = c.originPoint-c.centerPoint
          val v = u << (beta * (if(c.antiClockWise) 1.0 else -1.0))
          c.centerPoint + v
      }

      ElementPoint(p,None,e)

    }

    override def generatePoints(a: TEjeElement): List[ElementPoint] = {
      a match {
        case f: TFaintElement => List(ElementPoint(f.from,None,f),ElementPoint(f.end,None,f))
        case _ => calcPointToDistance(a,0) ::
          calcPointToDistance(a,a.length) ::
          (1 to a.length.toInt by 5).map(d => calcPointToDistance(a,d)).toList
      }

    }
  }

  def generatePoints[A <: TEjeElement](value: A)(implicit conv: EjeElement2PointsGenerator[A]): List[ElementPoint] = conv.generatePoints(value)

}
