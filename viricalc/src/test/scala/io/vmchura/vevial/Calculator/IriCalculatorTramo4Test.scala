package io.vmchura.vevial.Calculator

import io.vmchura.vevial.EjeVialBuilder.LandXMLToEje
import io.vmchura.vevial.EjeVialUtil.Progresiva
import io.vmchura.vevial.elementdata.IRIElementData
import io.vmchura.vevial.relevamiento.RelevamientoIRI
import org.scalatest.FlatSpec

import scala.io.Codec
import scala.reflect.io.File

class IriCalculatorTramo4Test extends FlatSpec {

  behavior of "Iri Calculator Tramo 4"

  it should "build correctly" in {
    val fileXML = File("/home/vmchura/Documents/003.CVSC/IRI/001.DataEjeVial/tramo45.xml")
    val ejeEither =  new LandXMLToEje(fileXML.reader(Codec("UTF-8"))).toEje

    assert(ejeEither.isRight)

    val eje = ejeEither match {
      case Right(value) => value
      case _ => null
    }

    assert(eje != null)
    println(eje.minProg)
    println(eje.maxProg)
    println(eje.length)

  }
  behavior of "Iri Calculator Tramo 6"

  it should "build correctly" in {
    val fileXML = File("/home/vmchura/Documents/003.CVSC/IRI/001.DataEjeVial/tramo67.xml")
    val ejeEither =  new LandXMLToEje(fileXML.reader(Codec("UTF-8"))).toEje

    assert(ejeEither.isRight)

    val eje = ejeEither match {
      case Right(value) => value
      case _ => null
    }

    assert(eje != null)
    println(eje.minProg)
    println(eje.maxProg)
    println(eje.length)

  }
}
