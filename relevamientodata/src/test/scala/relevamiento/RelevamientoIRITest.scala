package relevamiento



import java.io.File

import org.scalatest.FlatSpec

class RelevamientoIRITest extends FlatSpec {
    behavior of "Building relevamiento IRI"

  it should "end with no errors" in {
    val fileCSV = new File("/home/vmchura/Documents/001.Projects/Vevial/relevamientodata/src/test/resources/2019-03-05 14h36m22s Survey.csv")


    val relevamientoIRI = new RelevamientoIRI(fileCSV)

    assertResult(92)(relevamientoIRI.elements.length)
  }
}
