package io.vmchura.vevial.EjeVialBuilder



import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Codec
import scala.reflect.io.File

class LandXMLToEjeTest extends AnyFlatSpec with Matchers{

  behavior of "Creation and build of XMLToEje"

  it should "build correctly" in {
    val file = File("/home/vmchura/Documents/001.Projects/vevial/ejevialview/src/test/resources/testTramo.xml")
    val xmlBuiler = new LandXMLToEje(file.reader(Codec("UTF-8")))
    val result = xmlBuiler.toEje
    result match {
      case Right(value) =>
        assertResult(6)(value.elements.length)
        value.length should equal (189.674438876884 +- 1e-3)
        value.minProg should equal (66725.000000000175 +- 1e-3)
        value.maxProg should equal ((66725.000000000175+189.674438876884) +- 1e-3)

      case Left(errros) => println(errros)
    }
  }
}
