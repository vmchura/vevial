package io.vmchura.vevial.PlanarGeometric.BasicEje

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{
  PointUnitaryVector,
  TPoint
}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{
  ElementPoint,
  TEjeElement,
  TSimpleEjeElement
}
import io.vmchura.vevial.PlanarGeometric.ConfigParametersGeometric

trait TEfficientSeqEjeElements extends TSeqEjeElementsBase {
  val elements: List[TEjeElement]
  private lazy val elementsAsArray = elements.toArray
  lazy val elements2Indx: Map[TEjeElement, Int] = elements.zipWithIndex.map {
    case (e, i) => e -> i
  }.toMap
  import ConfigParametersGeometric.distanceToFindProjection
  import PointsOnElementGenerator._

  override def append(o: TSeqEjeElementsBase): TSeqEjeElementsBase =
    throw new NotImplementedError()

  private lazy val points: Seq[ElementPoint] =
    elements.flatMap(e => PointsOnElementGenerator.generatePoints(e))

  private lazy val pointsSortedByX = points.sortBy(_.x).toArray
  private lazy val pointsSortedByY = points.sortBy(_.y).toArray

  implicit val extractorX: ElementPoint => Double = _.x
  implicit val extractorY: ElementPoint => Double = _.y

  private lazy val acumLengths: Map[TEjeElement, Double] = {
    elements.tail
      .scanLeft((elements.head, 0.0: Double)) {
        case ((prevElement, acum), currentElement) =>
          (currentElement, acum + prevElement.length)
      }
      .map {
        case (element, acumToLeft) => element -> acumToLeft
      }
      .toMap
  }

  private lazy val (fX, fY) =
    List((extractorX, pointsSortedByX), (extractorY, pointsSortedByY)).map {
      case (e, orderedList) =>
        (d: Double) =>
          SubsequenceFinder.find[ElementPoint](
            distanceToFindProjection,
            distanceToFindProjection
          )(orderedList)(d)(e)
    } match {
      case first :: second :: Nil => (first, second)
      case _                      => (null, null)
    }

  override lazy val length: Double = elements.map(_.length).sum
  override lazy val in: PointUnitaryVector = elements.head.in
  override lazy val out: PointUnitaryVector = elements.last.out

  override def projectPoint(point: TPoint): Option[ElementPoint] = {

    (for {
      (xIni, xEnd) <- fX(point.x)
      (yIni, yEnd) <- fY(point.y)
    } yield {

      val closeByX = (xIni to xEnd)
        .map(i => elements2Indx(pointsSortedByX(i).ejeElementOwner))
        .toSet
      val closeByY = (yIni to yEnd)
        .map(i => elements2Indx(pointsSortedByY(i).ejeElementOwner))
        .toSet

      val elementsAround =
        (closeByX intersect closeByY).map(i => elementsAsArray(i)).toList

      //val elementsAround = intersection.map(_.ejeElementOwner)

      //val elementsAround = ((fX(point.x) intersect fY(point.y)) groupBy( _.ejeElementOwner) ).keySet.toList

      EfficientSeqEjeElements.bruteForceCalculation(elementsAround, point)

    }).flatten

  }

  override def pointIsInsideElement(point: TPoint): Boolean =
    projectPoint(point).exists(ep => ep.toSource.isEmpty)

  override def lengthToPoint(epoint: ElementPoint): Double = {
    val ElementPoint(_, _, owner) = epoint
    val result = acumLengths
      .get(owner)
      .map { baseLength =>
        baseLength + owner.lengthToPoint(epoint)
      }
      .getOrElse(throw new IllegalStateException())
    result
  }
  override lazy val leftmostPoint: TPoint =
    elements.map(_.leftmostPoint).minBy(_.x)

  override lazy val rightmostPoint: TPoint =
    elements.map(_.rightmostPoint).maxBy(_.x)

  override lazy val upperPoint: TPoint = elements.map(_.upperPoint).maxBy(_.y)

  override lazy val lowerPoint: TPoint = elements.map(_.lowerPoint).minBy(_.y)

}

case class EfficientSeqEjeElements(elements: List[TSimpleEjeElement])
    extends TEfficientSeqEjeElements {
  require(elements.nonEmpty, "EfficientSeqEjeElements")

}
object EfficientSeqEjeElements {

  def apply(elements: List[TSimpleEjeElement]): EfficientSeqEjeElements =
    new EfficientSeqEjeElements(elements)
  def apply(seqIneficient: TSeqEjeElementsBase): EfficientSeqEjeElements =
    seqIneficient match {
      case EmptySeqEjeElements() => throw new IllegalArgumentException()
      case NonEmptySeqEjeElements(elements) =>
        new EfficientSeqEjeElements(elements)
    }

  def bruteForceCalculation(
      elementsAround: Seq[TEjeElement],
      point: TPoint,
      debug: Boolean = false
  ): Option[ElementPoint] = {
    def printDebug(message: String, tag: String = "-"): Unit =
      if (debug)
        println(
          s"EfficientSeqEjeElements.bruteForceCalculation : [$tag] => $message"
        )
    if (elementsAround.nonEmpty) {
      val points10m = elementsAround
        .filter(e => (e.in.point - point).magnitude <= 10)
        .mkString(" --\n")
      printDebug(points10m, "< 10")

      val projections = elementsAround.flatMap(_.projectPoint(point))

      printDebug(projections.length.toString, "|projections|")

      val (exacts, inexacts) = projections.partition(_.toSource.isEmpty)

      printDebug(exacts.length.toString, "|exacts|")
      printDebug(inexacts.length.toString, "|inexacts|")
      val result = if (exacts.nonEmpty) {
        exacts.headOption

      } else {
        if (inexacts.isEmpty)
          None
        else
          Some(inexacts.minBy(_.toSource.get.magnitude))
      }

      result
    } else {
      None
    }
  }
}
