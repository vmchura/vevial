package PlanarGeometric.ProgresiveEje

import PlanarGeometric.BasicEje.TEfficientSeqEjeElements
import PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector}
import PlanarGeometric.ConfigParametersGeometric.areCloseInLinearReference
import PlanarGeometric.EjeElement._
import PlanarGeometric.RestrictiveEje._

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
  lazy val minProg: Double = projectPoint(in.point).map{ep => calcProgresive(ep)}.getOrElse(throw  new IllegalArgumentException())
  lazy val maxProg: Double = projectPoint(out.point).map{ep => calcProgresive(ep)}.getOrElse(throw  new IllegalArgumentException())
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
    if(linearRestricctions == null){
      println("is null in +"+this)
    }
    if(linearRestricctions.length<2)
      println(s"length of linearRes ${linearRestricctions.length}")
    val segmentResultOpt = linearRestricctions.zip(linearRestricctions.tail).find{
      case (m,n) => m.distanceFromReference <= dx && dx <= n.distanceFromReference}


    assert(segmentResultOpt.isDefined)
    segmentResultOpt.get match {
      case (LinearRestriction(di,pi), LinearRestriction(dj,pj)) =>
        {
          (dx-di)*(pj-pi)/(dj-di)+pi
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
trait TEfficientSeqEjeElementsProgresiva extends TEfficientSeqEjeElements with WithProgresive

case class RectSegmentProgresiva(originPoint: Point, endPoint: Point, restrictions: List[Restriction])
  extends TRectSegmentProgresiva

case class CircleSegmentProgresiva(originPoint: Point, centerPoint: Point, endPoint: Point, antiClockWise: Boolean, restrictions: List[Restriction])
  extends TCircleSegmentProgresiva

case class FaintSegmentProgresiva(from: Point, end: Point,restrictions: List[Restriction])
  extends TFaintSegmentProgresiva




