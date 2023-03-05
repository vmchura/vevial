package io.vmchura.vevial.relevamiento



import java.io.File

import io.vmchura.vevial.elementdata.IRIElementData
import org.scalatest.flatspec.AnyFlatSpec

class RelevamientoIRITest extends AnyFlatSpec {
    behavior of "Building io.vmchura.vevial.relevamiento IRI"
  val existCSVFile = false
  it should "end with no errors" in {
    assume(existCSVFile)
    val fileCSV = new File("/home/vmchura/Documents/001.Projects/vevial/relevamientodata/src/test/resources/2019-03-05 14h36m22s Survey.csv")


    val relevamientoIRI = RelevamientoIRI(fileCSV,cd => IRIElementData(cd))

    assertResult(92)(relevamientoIRI.elements.length)
  }

}
