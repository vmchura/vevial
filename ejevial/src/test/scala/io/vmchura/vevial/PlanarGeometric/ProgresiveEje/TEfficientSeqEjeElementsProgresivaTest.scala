package io.vmchura.vevial.PlanarGeometric.ProgresiveEje

import io.vmchura.vevial.EjeVialBuilder.LandXMLToEje
import org.scalatest.flatspec.AnyFlatSpec

import java.io
import scala.io.Codec
import scala.reflect.io.File

class TEfficientSeqEjeElementsProgresivaTest extends AnyFlatSpec {
  behavior of "TEfficientSeqEjeElementsProgresiva"
  it should "generate kml file" in {
    val file = File("/home/vmchura/Documents/001.Projects/vevial/ejevialview/src/test/resources/testTramo.xml")
    val xmlBuiler = new LandXMLToEje(file.reader(Codec("UTF-8")))
    val resultPath = new java.io.File("/home/vmchura/Documents/001.Projects/vevial/ejevialview/src/test/resources/testTramo.kml")
    val result = xmlBuiler.toEje.map(_.exportKML(resultPath))
    result match {
      case Right(_) =>
        assert(java.nio.file.Files.exists(resultPath.toPath))
      case Left(error) =>
        println(error)
        fail("testTramo.xml is not a valid eje")
    }
  }
}
