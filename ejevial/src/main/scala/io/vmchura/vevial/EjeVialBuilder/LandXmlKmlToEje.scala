package io.vmchura.vevial.EjeVialBuilder

import com.typesafe.scalalogging.Logger

import java.io.InputStreamReader
import com.scalakml.io.KmlFromXml
import com.scalakml.kml.{Coordinate, Document, FeaturePart, Folder, HexColor, LineString, Placemark, Point}
import io.vmchura.vevial.EjeVialUtil.{Coordinates, Progresiva}
import io.vmchura.vevial.PlanarGeometric.BasicEje.{EfficientSeqEjeElements, EmptySeqEjeElements, TSeqEjeElementsBase}
import io.vmchura.vevial.PlanarGeometric.EjeElement.RectSegment
import io.vmchura.vevial.PlanarGeometric.RestrictiveEje.ProgresivePoint

import java.security.InvalidParameterException
import scala.xml.XML

class LandXmlKmlToEje(source: InputStreamReader, kmlSource: InputStreamReader) extends LandXMLToEje(source) {
  private val xml = XML.load(kmlSource)
  private val kml = KmlFromXml.makeKml(xml)

  private val progressivePoints: Either[Exception, List[ProgresivePoint]] = kml.fold(Left(new InvalidParameterException("Cant parse")): Either[Exception, List[ProgresivePoint]]) {
    kmlParsed =>
      val progressivePoints = for {
        feature <- kmlParsed.feature
        document <- Option.when(feature.isInstanceOf[Document])(feature.asInstanceOf[Document])
      } yield {
        val folders = document.features.filter(_.isInstanceOf[Folder]).map(_.asInstanceOf[Folder])
        folders.flatMap { folder =>
          folder.features.flatMap { subFeature =>
            Option.when(subFeature.isInstanceOf[Placemark]) {
              val placeMark = subFeature.asInstanceOf[Placemark]
              for {
                geometry <- placeMark.geometry
                pointKML <- Option.when(geometry.isInstanceOf[Point])(geometry.asInstanceOf[Point])
                coordinatesKML <- pointKML.coordinates
                latitudeKML <- coordinatesKML.latitude
                longitudKML <- coordinatesKML.longitude
                progresivaStr <- placeMark.featurePart.name
                progresivaInt <- Progresiva(progresivaStr)
              } yield {
                val coordinateEje = Coordinates(latitud = latitudeKML, longitud = longitudKML).toUTMCoordinates()
                new ProgresivePoint(coordinateEje.toPoint(), progresivaInt.progresiva)
              }
            }
          }.flatten
        }.sortBy(_.progresive)
      }
      progressivePoints.fold(Left(new InvalidParameterException("No points found")) :Either[Exception, List[ProgresivePoint]])(sequenceProgressivePoints => Right(sequenceProgressivePoints.toList))
  }


  override protected def getSequenceProgresivePoint: Iterable[ProgresivePoint] =

    (for {
      sequenceProgressivePoints <- progressivePoints
      xmlParsing <- resultOfParsing
    } yield {
      val (listaXmlParsing, start) = xmlParsing
      val listaProg = sequenceProgressivePoints.filter(pp => pp.progresive>start && (pp.progresive.toInt%1000==0))
      (new ProgresivePoint(listaXmlParsing.head.in.point,start) :: listaProg)//.foreach(pp => println(s"$pp ${pp.progresive}"))
      //List(new ProgresivePoint(listaXmlParsing.head.in.point,start))// :: listaProg
    }).getOrElse(Nil)
}
