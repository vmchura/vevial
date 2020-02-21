package io.vmchura.vevial.Calculator


import io.vmchura.vevial.EjeVialBuilder.LandXMLToEje
import io.vmchura.vevial.EjeVialUtil.Progresiva
import io.vmchura.vevial.elementdata.IRIElementData
import io.vmchura.vevial.relevamiento.RelevamientoIRI
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Codec
import scala.reflect.io.File

class IriCalculatorTest extends AnyFlatSpec {

  behavior of "Iri Calculator"

  it should "build correctly" in {
    val fileCSV = new java.io.File("/home/vmchura/Documents/001.Projects/vevial/relevamientodata/src/test/resources/2019-03-05 14h36m22s Survey.csv")
    val fileXML = File("/home/vmchura/Documents/001.Projects/vevial/ejevialview/src/test/resources/tramo123.xml")
    val ejeEither =  new LandXMLToEje(fileXML.reader(Codec("UTF-8"))).toEje

    assert(ejeEither.isRight)

    val eje = ejeEither match {
      case Right(value) => value
      case _ => null
    }

    val relevamientoIRI = RelevamientoIRI(fileCSV,cd => IRIElementData(cd))

    val iriCalculator = new IriCalculator(eje,Progresiva(0),Progresiva(1e7.toInt))

  }
}
