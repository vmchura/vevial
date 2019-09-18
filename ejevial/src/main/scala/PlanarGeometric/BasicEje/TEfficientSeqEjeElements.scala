package PlanarGeometric.BasicEje

import PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector}
import PlanarGeometric.EjeElement.{ElementPoint, TEjeElement, TSimpleEjeElement}
import PlanarGeometric.ConfigParametersGeometric


trait TEfficientSeqEjeElements extends TSeqEjeElementsBase {
  def elements: List[TEjeElement]

  import ConfigParametersGeometric.distanceToFindProjection
  import SubsequenceFinder._
  import PointsOnElementGenerator._

  override def append(o: TSeqEjeElementsBase): TSeqEjeElementsBase= throw new NotImplementedError()



  private val points = elements.flatMap(e => PointsOnElementGenerator.generatePoints(e))

  private val pointsSortedByX = points.sortBy(_.x).toArray
  private val pointsSortedByY = points.sortBy(_.y).toArray
  implicit def extractorX: ElementPoint => DoubleSimpleReference = _.x
  implicit def extractorY: ElementPoint => DoubleSimpleReference = _.y

  private val acumLengths: Map[TEjeElement,Double] = {
    elements.tail.scanLeft((elements.head,0.0: Double)){case ((prevElement,acum),currentElement) => (currentElement,acum+prevElement.length)}.map{
      case (element, acumToLeft) => element -> acumToLeft
    }.toMap
  }

  private val (fX,fY) = List((extractorX,pointsSortedByX),(extractorY,pointsSortedByY)).map{case (e,orderedList) => (d: Double) =>
    SubsequenceFinder.find[ElementPoint,DoubleSimpleReference](distanceToFindProjection,distanceToFindProjection)(orderedList) (d) (e)} match {
    case first :: second :: Nil => (first,second)
    case _ => (null,null)
  }


  override val length: Double = elements.map(_.length).sum
  override val in: PointUnitaryVector = elements.head.in
  override val out: PointUnitaryVector = elements.last.out

  override def projectPoint(point: Point): Option[ElementPoint] = {
    val elementsAround = ((fX(point.x) intersect fY(point.y)) groupBy( _.ejeElementOwner) ).keySet.toList


    val projections = elementsAround.flatMap(_.projectPoint(point))

    val (exacts,inexacts) =  projections.partition(_.toSource.isEmpty)

    if(exacts.nonEmpty){
      exacts.headOption

    }else{

      inexacts.minByOption(_.toSource.get.magnitude)
    }

  }


  override def pointIsInsideElement(point: Point): Boolean = projectPoint(point).exists(ep => ep.toSource.isEmpty)

  override def lengthToPoint(point: ElementPoint): Double = {
    acumLengths.get(point.ejeElementOwner).map{ ep =>
    ep + point.ejeElementOwner.lengthToPoint(point)
  }.getOrElse(throw new IllegalStateException())}

}

case class EfficientSeqEjeElements(elements: List[TSimpleEjeElement]) extends TEfficientSeqEjeElements {
  require(elements.nonEmpty)

}
object EfficientSeqEjeElements{
  def apply(elements: List[TSimpleEjeElement]): EfficientSeqEjeElements = new EfficientSeqEjeElements(elements)
  def apply(seqIneficient: TSeqEjeElementsBase): EfficientSeqEjeElements = seqIneficient match {
    case EmptySeqEjeElements() => throw new IllegalArgumentException()
    case NonEmptySeqEjeElements(elements) => new EfficientSeqEjeElements(elements)
  }
}