package io.vmchura.vevial.Calculator.models.singularitypoint

import java.io.File

import org.scalatest.FlatSpec

class SingularityPointReaderTest extends FlatSpec {

  behavior of "Singularity Point"
  it should "load successfuly" in {
    val file = new File("/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/singularitypointsbasic.csv")
    val reader = new SingularityPointReader(file)
    reader.elements.foreach(l => println(SingularityPointRaw(0,l,null)))
  }
}
