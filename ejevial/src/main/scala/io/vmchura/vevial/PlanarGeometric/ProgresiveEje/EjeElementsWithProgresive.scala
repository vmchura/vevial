package io.vmchura.vevial.PlanarGeometric.ProgresiveEje

import io.vmchura.vevial.EjeVialUtil.Coordinates
import io.vmchura.vevial.PlanarGeometric.BasicEje.TEfficientSeqEjeElements
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import io.vmchura.vevial.PlanarGeometric.ConfigParametersGeometric.areCloseInLinearReference
import io.vmchura.vevial.PlanarGeometric.EjeElement._
import io.vmchura.vevial.PlanarGeometric.RestrictiveEje._

import java.io.File

trait WithProgresive extends TEjeElement {
  def calcProgresive(ep: ElementPoint): Double
  def findPointByProgresive(progresive: Int): Option[TPoint] = {


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
  private def calculateKnowPointProjection(point: TPoint): Double = {
    projectPoint(point).map{ep => calcProgresive(ep)}.getOrElse(throw new IllegalArgumentException())
  }
  lazy val minProg: Double = calculateKnowPointProjection(in.point)
  lazy val maxProg: Double = calculateKnowPointProjection(out.point)


}

trait WithDistributionFormulaByRestrictions extends WithProgresive {
  def restrictions: List[Restriction]
  assert(restrictions.length>=2)
  case class LinearRestriction(distanceFromReference: Double, progresive: Double)
  val linearRestricctions: List[LinearRestriction] = restrictions.map(r => LinearRestriction(lengthToPoint(r.elementPoint),r.progresive)).sortBy(_.distanceFromReference)
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
      segmentResultOpt match {
        case Some((LinearRestriction(di,pi), LinearRestriction(dj,pj))) =>

          if(areCloseInLinearReference(dj,di)){
            pi
          }else {
            (dx - di) * (pj - pi) / (dj - di) + pi
          }
        case None => throw new IllegalStateException("segment should be found")

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
      case r: TRectSegment => RectSegmentProgresiva(r.originPoint, r.endPoint, value.restrictions)
    }
  implicit val r2f_CircConverter: ConverterWithDistrutionFormula[CircleSegmentRestrictions, CircleSegmentProgresiva] =
    (value: CircleSegmentRestrictions) => value.element match {
      case c: TCircleSegment => CircleSegmentProgresiva(c.originPoint, c.centerPoint, c.endPoint, c.antiClockWise, value.restrictions)
    }

  implicit val r2f_FaintConverter: ConverterWithDistrutionFormula[FaintSegmentRestrictions, FaintSegmentProgresiva] =
    (value: FaintSegmentRestrictions) => value.element match {
      case f: TFaintElement =>  FaintSegmentProgresiva(f.from, f.end, value.restrictions)
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
  def exportKML(fileOutputPath: String): Unit = {
    import com.scalakml.kml.Kml
    import com.scalakml.kml.LineString
    import com.scalakml.kml.Coordinate
    import com.scalakml.kml.FeaturePart
    import com.scalakml.kml.Placemark
    import com.scalakml.io.KmlPrintWriter
    import xml.PrettyPrinter
    def tpoint2Coordinate(tPoint: TPoint): Coordinate = {
      val geodesic = Coordinates('L',18,tPoint.x,tPoint.y).toGeodesicCoordinates()
      new Coordinate(geodesic.longitude,geodesic.latitude,0)
    }
    val coordinates = elements.map(_.out.point)
    val lineString = LineString(coordinates = Option(elements.map(_.in.point).map(tpoint2Coordinate)))
    // create a Placemark with the point, and a name
    val placemark = Placemark(Option(lineString), FeaturePart(name = Option("TramoKML")))
    // create a kml root object with the placemark

    val kml = Kml(feature = Option(placemark))
    new KmlPrintWriter(fileOutputPath).write(Option(kml), new PrettyPrinter(80, 3))
  }
}

case class RectSegmentProgresiva(originPoint: TPoint, endPoint: TPoint, restrictions: List[Restriction])
  extends TRectSegmentProgresiva

case class CircleSegmentProgresiva(originPoint: TPoint, centerPoint: TPoint, endPoint: TPoint, antiClockWise: Boolean, restrictions: List[Restriction])
  extends TCircleSegmentProgresiva

case class FaintSegmentProgresiva(from: TPoint, end: TPoint,restrictions: List[Restriction])
  extends TFaintSegmentProgresiva




