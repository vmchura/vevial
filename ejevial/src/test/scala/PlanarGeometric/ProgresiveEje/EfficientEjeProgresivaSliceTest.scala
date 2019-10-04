package PlanarGeometric.ProgresiveEje

import EjeVialBuilder.LandXMLToEje
import org.scalatest.FlatSpec

import scala.io.Codec
import scala.reflect.io.File

class EfficientEjeProgresivaSliceTest extends FlatSpec {
  behavior of "Slice eje"

  it should "work fine slicing data" in {
    val fileXML = File("/home/vmchura/Documents/001.Projects/Vevial/ejevialview/src/test/resources/tramo123.xml")
    val ejeEither =  new LandXMLToEje(fileXML.reader(Codec("UTF-8"))).toEje

    assert(ejeEither.isRight)

    val eje = ejeEither match {
      case Right(value) => value
      case _ => null
    }

    println(s"${eje.minProg} => ${eje.maxProg}")
    println("slicing [95500,96750]")
    val slicing = eje.slice(95500,96750)
    println(s"${slicing.minProg} => ${slicing.maxProg}")
  }
}
