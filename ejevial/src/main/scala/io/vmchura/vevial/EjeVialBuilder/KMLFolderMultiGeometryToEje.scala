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


class KMLFolderMultiGeometryToEje(label: String)  extends EfficientEjeByPoints {

  val kmlSeq = new KmzFileReader().getKmlFromKmzFile("D:/ProjectAnita/pgv.kmz")

  def parseProgresivas(features: Seq[Feature]): Seq[(Progresiva, GeodesicCoordinates)] = {
    features.flatMap {
      case f: Folder if f.featurePart.name.get.startsWith("Text") =>
        f.features.toList match {
          case (pm: Placemark) :: Nil => pm.geometry.flatMap {
            case p: Point => for {
              progresiva <- pm.featurePart.name
              coordinate <- p.coordinates
              latitude <- coordinate.latitude
              longitude <- coordinate.longitude
              progresivaValue <- Progresiva(progresiva)
            } yield {
              (progresivaValue, Coordinates(latitude, longitude))
            }
            case _ => Option.empty
          }
          case _ => Option.empty
        }
      case f: Folder if f.featurePart.name.get.startsWith("PROG.") =>
        parseProgresivas(f.features)
      case _ => Option.empty
    }
  }

  def findProgresivas[M](feature: Feature, label: String)(implicit extractor: Seq[Feature] => Seq[M]): Seq[M] = {
    feature match {
      case f: Folder =>
        if (f.featurePart.name.contains(label)) {
          println(label)
          extractor(f.features)
        } else {
          f.features.flatMap(singleFeature => findProgresivas(singleFeature, label))
        }
      case _ => Nil
    }
  }

  def parseEje(features: Seq[Feature]): Seq[Seq[Seq[GeodesicCoordinates]]] = {
    features.flatMap {
      case f: Folder if f.featurePart.name.get.startsWith("Polyline") =>
        f.features.map {
          case pm: Placemark =>
            pm.geometry.map {
              case (mg: MultiGeometry) => mg.geometries.map {
                case ls: LineString => ls.coordinates.map { coordinates =>
                  coordinates.flatMap { coordinate =>
                    for {
                      latitude <- coordinate.latitude
                      longitude <- coordinate.longitude
                    } yield {
                      Coordinates(latitude, longitude)
                    }
                  }
                }.getOrElse(Nil)
                case _ => Nil
              }
              case _ => Nil
            }.getOrElse(Nil)
          case _ => Nil
        }
      case f: Folder if f.featurePart.name.get.startsWith("EJE") =>
        parseEje(f.features)
      case _ => Option.empty
    }
  }

  val progressiveDistanceMap = kmlSeq.flatten.flatMap(_.feature).flatMap(f => findProgresivas(f, label)(parseProgresivas)).toArray
  val ejeMap = kmlSeq.flatten.flatMap(_.feature).flatMap(f => findProgresivas(f, label)(parseEje)).flatten.toArray.map(_.map(_.toUTMCoordinates()))

  val innerEfficientEjeByPoints = EfficientEjeByPoints(label, ejeMap, progressiveDistanceMap)

  override def findProgresiva(point: TPoint): Option[(Option[String], Double, TPoint)] = innerEfficientEjeByPoints.findProgresiva(point)
}

