package io.vmchura.vevial.EjeVialBuilder

import com.scalakml.io.KmzFileReader
import com.scalakml.kml._
import io.vmchura.vevial.EjeVialUtil._
import io.vmchura.vevial.PlanarGeometric.BasicEje.{EfficientByPoints, EfficientSeqEjeElements, EmptySeqEjeElements, PointsOnElementGenerator, TSeqEjeElementsBase}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{RectSegment, TEjeElement, TRectSegment, TSimpleEjeElement}
import io.vmchura.vevial.PlanarGeometric.RestrictiveEje.ProgresivePoint
import PointsOnElementGenerator._
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import io.vmchura.vevial.PlanarGeometric.ConfigParametersGeometric

import scala.util.{Failure, Success, Try}
case class Node(index: Int, next: Int, distanceNext: Double)

class KMLFolderMultiGeometryToEje(label: String, placeMarkNames: List[String])  extends EfficientEjeByPoints {

  val kmlSeq = new KmzFileReader().getKmlFromKmzFile("D:/ProjectAnita/odnas.kmz")

  def parseProgresivas(features: Seq[Feature]): Seq[(Progresiva, GeodesicCoordinates)] = {
    features.toList.flatMap {
      case pm: Placemark => Some(pm)
      case _ => Option.empty[Placemark]
    } flatMap { pm: Placemark =>
      pm.geometry.flatMap {
        case p: Point => for {
          progresiva <- pm.featurePart.name
          coordinate <- p.coordinates
          latitude <- coordinate.latitude
          longitude <- coordinate.longitude
          progresivaValue <- Progresiva(progresiva)
        } yield {
          (progresivaValue, Coordinates(latitude, longitude))
        }
        case _ => None
      }
    }
  }

  def findProgresivas[M](feature: Feature, label: String)(implicit extractorProgresivas: Seq[Feature] => Seq[M]): Seq[M] = {
    feature match {
      case f: Folder =>
        if (f.featurePart.name.contains(label)) {
          println(label)
          extractorProgresivas(f.features)
        } else {
          Nil
        }
      case _ => Nil
    }
  }

  def findEje[M](feature: Feature)(implicit extractorEje: Geometry => Seq[M]): Option[(String, Seq[M])] = {
    feature match {
      case pm: Placemark =>
        (for{
          name <- pm.featurePart.name
          _ <- Option.when(placeMarkNames.contains(name))(true)
          geometry <- pm.geometry
        }yield{
          (name, extractorEje(geometry))
        })
      case _ => None
    }
  }

  def parseEje(geometry: Geometry): Seq[GeodesicCoordinates] = {

    geometry match {
      case (mg: MultiGeometry) => mg.geometries.flatMap {
        case ls: LineString => ls.coordinates.toList.flatMap {
          case c :: Nil =>

            for {
              latitude <- c.latitude
              longitude <- c.longitude
            } yield {
              Coordinates(latitude, longitude)
            }
          case _ => None

        }
        case _ => Nil
      }
      case _ => Nil

    }
  }

  val progressiveDistanceMap = kmlSeq.flatten.flatMap(_.feature).flatMap(f => findProgresivas(f, label)(parseProgresivas)).toArray
  val ejeMap = kmlSeq.flatten.flatMap(_.feature).map(f => findEje(f)(parseEje))//.flatMap(f => findEje(f)(parseEje)).flatten.toArray.map(_.map(_.toUTMCoordinates()))



  def findClosest(index: Int): (Int, Double) = {
    val lastNode = ejeMap(index).last.toPoint()
    ejeMap.zipWithIndex.filter { case (_, i) => i != index }.map {
      case (other, i) =>
        val first = other.head.toPoint()
        (i, (lastNode - first).magnitude)
    }.minBy(_._2)
  }

  val nodes = ejeMap.indices.map(i => {
    val (next, distanceNext) = findClosest(i)
    Node(i, next, distanceNext)
  })
  val completeChainTrazabilidad = nodes.foldLeft(Set(ejeMap.indices: _*)) {
    case (prevSet, n) => prevSet.filterNot(_ == n.next)
  }.toList match {
    case initial :: Nil =>

      val visited = Array.fill(ejeMap.length)(false)

      val (_, result) = nodes.indices.foldLeft((initial, List.empty[Seq[(UTMCoordinates, Int, Int)]])) {
        case ((current, prev), _) =>
          if (visited(current)) {
            (current, prev)
          } else {
            val currentNode = nodes(current)
            visited(current) = true
            (currentNode.next, ejeMap(current).zipWithIndex.map{
              case (a,b) => (a,b,current)
            } :: prev)
          }

      }
      result.reverse.flatten
    case _ => Nil

  }
  val completeChain = completeChainTrazabilidad.map(_._1)
  println(completeChain.length)



  val elements: Seq[TEjeElement] = completeChain.zip(completeChain.tail).flatMap {
    case (prev, next) =>

      Try {
        RectSegment(prev.toPoint(), next.toPoint())
      } match {
        case Success(rs) => Some(rs)
        case Failure(_) => None
      }
  }
  val points = elements.flatMap(e  => PointsOnElementGenerator.generatePoints(e).map(_.point)).toArray
  val numberPoints = points.length
  val pointsMap = points.zipWithIndex.toMap




  val progArray = Array.fill(numberPoints)(Option.empty[Double])
  println(s"|points| ${points.length}")
  println(s"|progresivas| ${progressiveDistanceMap.length}")
  println(s"|*| ${((progressiveDistanceMap.length*points.length)/1e6).toLong}")
  val ejeByPoints = EfficientByPoints(points)
  progressiveDistanceMap.foreach{ case (progresiva, point) =>
    for{
      pointFound <- ejeByPoints.closestPoint(point.toPoint())
      index <- pointsMap.get(pointFound)
    }{
      progArray(index) = Some(progresiva.progresiva.toDouble)
    }
  }
  val nextIndexWithProgDefined: Array[Option[Int]] = Array.fill(numberPoints)(None)
  val previousIndexWithProgDefined: Array[Option[Int]] = Array.fill(numberPoints)(None)
  progArray.zipWithIndex.foldLeft(Option.empty[Int]){
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
  progArray.zip(previousIndexWithProgDefined).zip(nextIndexWithProgDefined).zipWithIndex.foreach{
    case (((Some(_), _), _), _) => ()
    case (((None, Some(prev)), None), i) => progArray(i) = progArray(prev).map(prevProg => sumAcumFromLeft(i) - sumAcumFromLeft(prev) + prevProg)
    case (((None, None), Some(next)), i) => progArray(i) = progArray(next).map(nextProg => nextProg -  (sumAcumFromLeft(next) - sumAcumFromLeft(i)))
    case (((None, Some(prev)), Some(next)), i) => progArray(i) = progArray(prev).flatMap{ prevProg =>
      progArray(next).map{ nextProg =>
        val lengthPrev = sumAcumFromLeft(i) - sumAcumFromLeft(prev)
        val lengthNext = sumAcumFromLeft(next) - sumAcumFromLeft(i)
        val deltaProg = nextProg - prevProg
        val offsetMari = (deltaProg*lengthPrev)/(lengthPrev + lengthNext)
        offsetMari + prevProg
      }
    }
  }

  override def findProgresiva(point: TPoint): Option[(Option[String], Double)] = {
    (for {
      pointFound <- ejeByPoints.closestPoint(point)
      index <- pointsMap.get(pointFound)
    } yield {
      progArray(index)
    }).flatten.map(x => (None, x))
  }

}

