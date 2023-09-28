package io.vmchura.vevial.EjeVialBuilder

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.scalakml.io.{KmlPrintWriter, KmzFileReader}
import com.scalakml.kml.{Feature, Folder, Kml, LineString, MultiGeometry, Placemark, Point}
import io.vmchura.vevial.EjeVialUtil.{Coordinates, GeodesicCoordinates, UTMCoordinates}

import java.io.StringWriter
import xml.PrettyPrinter

class KMLToEjeTest extends AnyFlatSpec with Matchers{
  behavior of "Creation and build of XMLToEje"

  it should "build correctly" in {
    println("....ReadKmzExample1 start...\n")
    import java.nio.file.{Paths, Files}
    import java.nio.charset.StandardCharsets


    // "TRAMO_3S-F_UTM"
    val ejeTramo3sBuilder = new KMLFolderMultiGeometryToEje("TRAMO_3S-F_UTM")
//    val resultData = ejeTramo3sBuilder.nodes.map{ case Node(i, next, distanceNext) =>
//      val initPoint = ejeTramo3sBuilder.ejeMap(i).head
//      val lastPoint = ejeTramo3sBuilder.ejeMap(i).last
//      val initNextPoint = ejeTramo3sBuilder.ejeMap(next).head
//      val lastNextPoint = ejeTramo3sBuilder.ejeMap(next).last
//      s"$i,$next,${initPoint.Easting},${initPoint.Northing},${lastPoint.Easting},${lastPoint.Northing},${initNextPoint.Easting},${initNextPoint.Northing},${lastNextPoint.Easting},${lastNextPoint.Northing},$distanceNext"
//
//    }.mkString(System.lineSeparator())
//
//    val chain = ejeTramo3sBuilder.completeChainTrazabilidad
//    val resultDataPoints = chain.zip(chain.tail.map(_._1)).map{
//      case ((a, i, j), b) => s"$i,$j,${(a.toPoint().-(b.toPoint())).magnitude.toString}"
//    }.mkString(System.lineSeparator())
//
//    Files.write(Paths.get("distance.csv"), resultData.getBytes(StandardCharsets.UTF_8))
//    Files.write(Paths.get("resultDataPoints.csv"), resultDataPoints.getBytes(StandardCharsets.UTF_8))
//    val ejeTramo3SF = ejeTramo3sBuilder.toEje
//    assert(ejeTramo3SF.isRight)
    val points = ejeTramo3sBuilder.progArray
    val resultDataPoints = points.map{ p =>
      s"${p.map(_.toString).getOrElse("-")}"
    }.mkString(System.lineSeparator())
    Files.write(Paths.get("resultDataPoints.csv"), resultDataPoints.getBytes(StandardCharsets.UTF_8))




  }
}
