package io.vmchura.vevial.EjeVialBuilder

import io.vmchura.vevial.PlanarGeometric.BasicEje.PointsOnElementGenerator
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import io.vmchura.vevial.PlanarGeometric.EjeElement.{RectSegment, TEjeElement}
import PointsOnElementGenerator._
import io.vmchura.vevial.EjeVialUtil.{GeodesicCoordinates, Progresiva, UTMCoordinates}
import io.vmchura.vevial.PlanarGeometric.BasicEje.EfficientByPoints

import scala.util.{Failure, Success, Try}

trait EfficientEjeByPoints {
  def findProgresiva(point: TPoint): Option[(Option[String], Double, TPoint)]
}

object EfficientEjeByPoints{

  def sortByClosePoints(points: Seq[Seq[TPoint]]): Seq[TPoint] = {
    val pointsArray = points.toArray
    def findClosest(index: Int): (Int, Double) = {
      val lastNode = pointsArray(index).last
      pointsArray.zipWithIndex.filter { case (_, i) => i != index }.map {
        case (other, i) =>
          val first = other.head
          (i, (lastNode - first).magnitude)
      }.minBy(_._2)
    }
    case class Node(index: Int, next: Int, distanceNext: Double)

    val nodes = pointsArray.indices.map(i => {
      val (next, distanceNext) = findClosest(i)
      Node(i, next, distanceNext)
    })
    def concatenateChain(initial: Int): Seq[(TPoint, Int, Int)] = {
      val visited = Array.fill(pointsArray.length)(false)

      val (_, result) = nodes.indices.foldLeft((initial, List.empty[Seq[(TPoint, Int, Int)]])) {
        case ((current, prev), _) =>
          if (visited(current)) {
            (current, prev)
          } else {
            val currentNode = nodes(current)
            visited(current) = true
            (currentNode.next, pointsArray(current).zipWithIndex.map {
              case (a, b) => (a, b, current)
            } :: prev)
          }

      }
      result.reverse.flatten
    }

    val completeChainTrazabilidad = nodes.foldLeft(Set(pointsArray.indices: _*)) {
      case (prevSet, n) => prevSet.filterNot(_ == n.next)
    }.toList match {
      case initial :: Nil =>
        concatenateChain(initial)
      case initials =>
        initials.flatMap(concatenateChain)
    }
    completeChainTrazabilidad.map(_._1)
  }

  def build_axis_points(completeChain: Seq[TPoint]): Array[TPoint] = {
    val elements: Seq[TEjeElement] = completeChain.zip(completeChain.tail).flatMap {
      case (prev, next) =>
        Try {
          RectSegment(prev, next)
        } match {
          case Success(rs) => Some(rs)
          case Failure(_) => None
        }
    }
    val points = elements.flatMap(e => PointsOnElementGenerator.generatePoints(e).map(_.point)).toArray
    points
  }

  def completePointsWithProgressive(completeChain: Seq[TPoint],
                                    progressiveDistanceMap: Array[(Progresiva, GeodesicCoordinates)]): (Array[Option[Double]], EfficientByPoints, Map[TPoint, Int]) = {
    val points = EfficientEjeByPoints.build_axis_points(completeChain)

    val numberPoints = points.length
    val pointsMap = points.zipWithIndex.toMap


    val progArray = Array.fill(numberPoints)(Option.empty[Double])
    println(s"|points| ${points.length}")
    println(s"|progresivas| ${progressiveDistanceMap.length}")
    println(s"|*| ${((progressiveDistanceMap.length * points.length) / 1e6).toLong}")
    val ejeByPoints = EfficientByPoints(points)
    progressiveDistanceMap.foreach { case (progresiva, point) =>
      for {
        pointFound <- ejeByPoints.closestPoint(point.toPoint())
        index <- pointsMap.get(pointFound)
      } {
        progArray(index) = Some(progresiva.progresiva.toDouble)
      }
    }
    val nextIndexWithProgDefined: Array[Option[Int]] = Array.fill(numberPoints)(None)
    val previousIndexWithProgDefined: Array[Option[Int]] = Array.fill(numberPoints)(None)
    progArray.zipWithIndex.foldLeft(Option.empty[Int]) {
      case (None, (None, _)) => None
      case (Some(prev), (None, i)) =>
        previousIndexWithProgDefined(i) = Some(prev)
        Some(prev)
      case (_, (Some(_), i)) =>
        previousIndexWithProgDefined(i) = Some(i)
        Some(i)
    }
    progArray.zipWithIndex.foldRight(Option.empty[Int]) {
      case ((None, _), None) => None
      case ((None, i), Some(next)) =>
        nextIndexWithProgDefined(i) = Some(next)
        Some(next)
      case ((Some(_), i), _) =>
        nextIndexWithProgDefined(i) = Some(i)
        Some(i)
    }

    val sumAcumFromLeft = Array.fill(numberPoints)(0.0)
    points.indices.tail.foreach { n =>
      val current = points(n)
      val prev = points(n - 1)
      sumAcumFromLeft(n) = sumAcumFromLeft(n - 1) + (current - prev).magnitude
    }
    progArray.zip(previousIndexWithProgDefined).zip(nextIndexWithProgDefined).zipWithIndex.foreach {
      case (((Some(_), _), _), _) => ()
      case (((None, Some(prev)), None), i) => progArray(i) = progArray(prev).map(prevProg => sumAcumFromLeft(i) - sumAcumFromLeft(prev) + prevProg)
      case (((None, None), Some(next)), i) => progArray(i) = progArray(next).map(nextProg => nextProg - (sumAcumFromLeft(next) - sumAcumFromLeft(i)))
      case (((None, Some(prev)), Some(next)), i) => progArray(i) = progArray(prev).flatMap { prevProg =>
        progArray(next).map { nextProg =>
          val lengthPrev = sumAcumFromLeft(i) - sumAcumFromLeft(prev)
          val lengthNext = sumAcumFromLeft(next) - sumAcumFromLeft(i)
          val deltaProg = nextProg - prevProg
          val offsetMari = (deltaProg * lengthPrev) / (lengthPrev + lengthNext)
          offsetMari + prevProg
        }
      }
    }
    (progArray, ejeByPoints, pointsMap)
  }

  def apply(label: String, ejeMap: Array[Seq[UTMCoordinates]], progressiveDistanceMap: Array[(Progresiva, GeodesicCoordinates)]): EfficientEjeByPoints = new EfficientEjeByPoints {
    println(s"$label: |ejeMap|: ${ejeMap.length}")
    println(s"$label: |progressiveDistanceMap|: ${progressiveDistanceMap.length}")
    val completeChain = EfficientEjeByPoints.sortByClosePoints(ejeMap.map(_.map(_.toPoint())))
    println(s"$label: |completeChain|: ${completeChain.length}")
    val (progArray, ejeByPoints, pointsMap) = EfficientEjeByPoints.completePointsWithProgressive(completeChain, progressiveDistanceMap)

    override def findProgresiva(point: TPoint): Option[(Option[String], Double, TPoint)] = {
      (for {
        pointFound <- ejeByPoints.closestPoint(point)
        index <- pointsMap.get(pointFound)
        progresiva <- progArray(index)
      } yield {
        (progresiva, pointFound)
      }).map(x => (Some(label), x._1, x._2))
    }

  }
}
