package io.vmchura.vevial.Calculator.models

import org.scalatest.FlatSpec

class FactorCorreccionCalculatorTest extends FlatSpec {

  behavior of "FactorCorreccionCalculatorTest"

  it should "calcFactorCorrection" in {
    val fileCSV1 = new java.io.File("/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/factorCorrecion.csv")
    val f = new FactorCorreccionCalculator(fileCSV1)
    val calculator = f.calcFactorCorrection
    assertResult(1.0)(calculator(50))
    assertResult(2.0)(calculator(10000+50))

  }

}
