package io.vmchura.vevial.PlanarGeometric.ProgresiveEje

import io.vmchura.vevial.PlanarGeometric.BasicEje.TEfficientSeqEjeElements
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point
import io.vmchura.vevial.PlanarGeometric.ConfigParametersGeometric.areCloseInLinearReference
import io.vmchura.vevial.PlanarGeometric.EjeElement._
import io.vmchura.vevial.PlanarGeometric.RestrictiveEje._

trait WithProgresive extends TEjeElement {
  def calcProgresive(ep: ElementPoint): Double
  def findPointByProgresive(progresive: Int): Option[Point] = {


    val isPossible = minProg <= progresive && progresive <= maxProg
    if(isPossible) {

        var left = 0d
        var right = length
        while(right-left >1e-4){
          val m = (left+right)/2d
          val epOpt = findPointByLength(m)
          assert(epOpt.isDefined)
          val ep = epOpt.get
          val prog = calcProgresive(ep)
          if(prog>progresive){
            right = m
          }else{
            left = m
          }
        }
        val epOpt = findPointByLength(left)
        assert(epOpt.isDefined)
        Some(epOpt.get.point)

    }else{
      None
    }
  }
  def findPointByLength(lengthParam: Double): Option[ElementPoint]
  private def calculateKnowPointProjection(point: Point): Double = {
    projectPoint(point).map{ep => calcProgresive(ep)}.getOrElse(throw new IllegalArgumentException())
  }
  lazy val minProg: Double = calculateKnowPointProjection(in.point)
  lazy val maxProg: Double = calculateKnowPointProjection(out.point)


}

trait WithDistributionFormulaByRestrictions extends WithProgresive {
  def restrictions: List[Restriction]
  assert(restrictions.length>=2)
  case class LinearRestriction(distanceFromReference: Double, progresive: Double)
  val linearRestricctions: List[LinearRestriction] = restrictions.map(r => LinearRestriction(lengthToPoint(r.elementPoint),r.progresive)).sortBy(_.progresive)
  assert(areCloseInLinearReference(linearRestricctions.head.distanceFromReference,0))
  assert(areCloseInLinearReference(linearRestricctions.last.distanceFromReference,length))

  override def calcProgresive(ep: ElementPoint): Double = {
    val dx = lengthToPoint(ep)

    val closestRestrictionToDx = linearRestricctions.find(lr => areCloseInLinearReference(lr.distanceFromReference,dx))
    if(closestRestrictionToDx.isDefined){
      closestRestrictionToDx.get.progresive
    }else{
      val segmentResultOpt = linearRestricctions.zip(linearRestricctions.tail).find{
        case (m,n) => m.distanceFromReference <= dx && dx <= n.distanceFromReference}


      assert(segmentResultOpt.isDefined)
      segmentResultOpt.get match {
        case (LinearRestriction(di,pi), LinearRestriction(dj,pj)) =>

          if(areCloseInLinearReference(dj,di)){
            pi
          }else {
            (dx - di) * (pj - pi) / (dj - di) + pi
          }

      }
    }



  }
}

trait WithDistributionFormula extends WithProgresive

trait ConverterWithDistrutionFormula[A <: WithRestrictionsIncremental[A,_],C <: WithProgresive]{
  def convert(value: A): C
}

object WithDistributionFormula{
  implicit val r2f_RectConverter: ConverterWithDistrutionFormula[RectSegmentRestrictions, RectSegmentProgresiva] =
    (value: RectSegmentRestrictions) => value.element match {
    case RectSegment(originPoint, endPoint) => RectSegmentProgresiva(originPoint, endPoint, value.restrictions)
  }
  implicit val r2f_CircConverter: ConverterWithDistrutionFormula[CircleSegmentRestrictions, CircleSegmentProgresiva] =
    (value: CircleSegmentRestrictions) => value.element match {
    case CircleSegment(origin, center, end, cc) => CircleSegmentProgresiva(origin, center, end, cc, value.restrictions)
  }

  implicit val r2f_FaintConverter: ConverterWithDistrutionFormula[FaintSegmentRestrictions, FaintSegmentProgresiva] =
    (value: FaintSegmentRestrictions) => value.element match {
    case FaintElement(origin, end) => FaintSegmentProgresiva(origin, end, value.restrictions)
  }


  def convert[A <: WithRestrictionsIncremental[A,_],
              C <: WithProgresive](value: A)(implicit conv: ConverterWithDistrutionFormula[A,C]): C =
    conv.convert(value)

}

trait TRectSegmentProgresiva extends TRectSegment with WithDistributionFormulaByRestrictions{
  override def findPointByLength(lengthParam: Double): Option[ElementPoint] = {
    if(lengthParam <0 || lengthParam > length){
      None
    }else{
      Some(ElementPoint(in.point+in.direction*lengthParam,None,this))
    }
  }


}
trait TCircleSegmentProgresiva extends TCircleSegment with WithDistributionFormulaByRestrictions {
  override def findPointByLength(lengthParam: Double): Option[ElementPoint] = {
    if(lengthParam <0 || lengthParam > length){
      None
    }else{
      val beta = lengthParam/radius * (if (antiClockWise) 1.0 else -1.0)
      Some(ElementPoint(centerPoint+ ((in.point-centerPoint)<<beta),None,this))
    }
  }
}
trait TFaintSegmentProgresiva extends  TFaintElement with WithDistributionFormulaByRestrictions {
  override def findPointByLength(length: Double): Option[ElementPoint] = None
}
trait TEfficientSeqEjeElementsProgresiva extends TEfficientSeqEjeElements with WithProgresive {
  def slice(progIni: Int, progFin: Int): TEfficientSeqEjeElementsProgresiva
}

case class RectSegmentProgresiva(originPoint: Point, endPoint: Point, restrictions: List[Restriction])
  extends TRectSegmentProgresiva

case class CircleSegmentProgresiva(originPoint: Point, centerPoint: Point, endPoint: Point, antiClockWise: Boolean, restrictions: List[Restriction])
  extends TCircleSegmentProgresiva

case class FaintSegmentProgresiva(from: Point, end: Point,restrictions: List[Restriction])
  extends TFaintSegmentProgresiva



