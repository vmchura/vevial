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

class KMLMultipleContinuosToEje(progresivasLabel: String, placeMarkNames: List[String])  extends EfficientEjeByPoints {

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
      case d: Document =>
        d.features.flatMap(f => findProgresivas(f, label))
      case _ => Nil

    }
  }

  def findEje[M](feature: Feature)(implicit extractorEje: Geometry => Seq[M]): Seq[(String, Seq[M])] = {
    feature match {
      case pm: Placemark =>
        (for{
          name <- pm.featurePart.name
          _ <- Option.when(placeMarkNames.contains(name))(true)
          geometry <- pm.geometry
        }yield{
          List((name, extractorEje(geometry)))
        }).getOrElse(Nil)
      case d: Document =>
        d.features.flatMap(f => findEje(f))
      case _ => Nil
    }
  }

  def parseEje(geometry: Geometry): Seq[List[GeodesicCoordinates]] = {

    geometry match {
      case (mg: MultiGeometry) => mg.geometries.map {
        case ls: LineString => ls.coordinates.map { _.flatMap{ coordinate =>

            for {
              latitude <- coordinate.latitude
              longitude <- coordinate.longitude
            } yield {
              Coordinates(latitude, longitude)
            }
          }}.getOrElse(Nil).toList
        case _ => Nil
      }
      case _ => Nil

    }
  }

  val progressiveDistanceMap = kmlSeq.flatten.flatMap(_.feature).flatMap(f => findProgresivas(f, progresivasLabel)(parseProgresivas)).toArray
  println(progressiveDistanceMap.length)

  val ejeMap = kmlSeq.flatten.flatMap(_.feature).flatMap(f => findEje(f)(parseEje))//.flatMap(f => findEje(f)(parseEje)).flatten.toArray.map(_.map(_.toUTMCoordinates()))
  println(progressiveDistanceMap.length)
  val ejesMade = ejeMap.map{
    case (tramoLabel, singleEjeMap) => EfficientEjeByPoints(tramoLabel, singleEjeMap.map(_.map(_.toUTMCoordinates())).toArray, progressiveDistanceMap)
  }

  override def findProgresiva(point: TPoint): Option[(Option[String], Double, TPoint)] = {
    ejesMade.flatMap(_.findProgresiva(point)).minByOption(r => r._3.-(point).magnitude)
  }

}

