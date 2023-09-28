package io.vmchura.vevial.PlanarGeometric.BasicEje

import com.scalakml.io.KmlFromXml
import com.scalakml.kml.{Coordinate, FeaturePart, HexColor, LineString, Placemark}
import io.vmchura.vevial.EjeVialUtil.Coordinates
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{PointUnitaryVector, TPoint}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{ElementPoint, TEjeElement, TSimpleEjeElement}
import io.vmchura.vevial.PlanarGeometric.ConfigParametersGeometric
import io.vmchura.vevial.PlanarGeometric.EjeElement.{RectSegment, TEjeElement, TRectSegment, TSimpleEjeElement}
import java.io.File
import scala.util.Try
import scala.util.{Failure, Success, Try}
trait TEfficientByPoints  {
  val basePoints: Array[TPoint]


  import ConfigParametersGeometric.distanceToFindProjection
  import PointsOnElementGenerator._



  private lazy val pointsSortedByX = basePoints.sortBy(_.x)
  private lazy val pointsSortedByY = basePoints.sortBy(_.y)

  implicit val extractorX: TPoint => Double = _.x
  implicit val extractorY: TPoint => Double = _.y



  private lazy val (fX, fY) =
    List((extractorX, pointsSortedByX), (extractorY, pointsSortedByY)).map {
      case (e, orderedList) =>
        (d: Double, distanceRange: Double) =>
          SubsequenceFinder.find[TPoint](
            distanceRange,
            distanceRange
          )(orderedList)(d)(e)
    } match {
      case first :: second :: Nil => (first, second)
      case _                      => (null, null)
    }


  def closestPoint(
      point: TPoint,
      distanceRange: Double = distanceToFindProjection
  ): Option[TPoint] = {
    (for {
      (xIni, xEnd) <- fX(point.x, distanceRange)
      (yIni, yEnd) <- fY(point.y, distanceRange)
    } yield {

      val closeByX = (xIni to xEnd)
        .map(i => pointsSortedByX(i))
        .toSet
      val closeByY = (yIni to yEnd)
        .map(i => pointsSortedByY(i))
        .toSet

      val elementsAround =
        (closeByX intersect closeByY).toList

      TEfficientByPoints.bruteForceCalculation(elementsAround, point)

    }).flatten
  }

}

case class EfficientByPoints(basePoints: Array[TPoint])
    extends TEfficientByPoints {
  require(basePoints.nonEmpty, "EfficientSeqEjeElements")

}
object TEfficientByPoints {

  def apply(elements: List[TSimpleEjeElement]): EfficientSeqEjeElements =
    new EfficientSeqEjeElements(elements)
  def apply(seqIneficient: TSeqEjeElementsBase): EfficientSeqEjeElements =
    seqIneficient match {
      case EmptySeqEjeElements() => throw new IllegalArgumentException()
      case NonEmptySeqEjeElements(elements) =>
        new EfficientSeqEjeElements(elements)
    }

  def bruteForceCalculation(
      elementsAround: Seq[TPoint],
      point: TPoint,
  ): Option[TPoint] = {

    Option.when (elementsAround.nonEmpty) {
      elementsAround.
        minByOption(e => (e-point).magnitude)
    }.flatten
  }
}
