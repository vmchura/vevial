package io.vmchura.vevial.EjeVialBuilder

import io.vmchura.vevial.EjeVialUtil.Coordinates
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Codec
import scala.reflect.io.File

class MultipleEjeProjectoSingularTest extends AnyFlatSpec with Matchers {

  val ejeODNA = new ODNAMultipleEjeProjectoSingular()
  val ejePGV = new PGVMultipleEjeProjectoSingular()
  behavior of "ODNAMultipleEjeProjectoSingular"
  it should "build correctly" in {
    ExpectedPoints.points.
      foreach { case (coordinate, expectedResults) =>
        val resultODNA = ejeODNA.findProgresiva(coordinate.toPoint())
        println(resultODNA)
        val resultPGV = ejePGV.findProgresiva(coordinate.toPoint())
        println(resultPGV)
        assert(resultODNA.exists {
          case (tramo, progresiva, _) =>
            progresiva >= expectedResults("ODNA")._1 &&
              progresiva <= expectedResults("ODNA")._2 &&
              tramo.contains(expectedResults("ODNA")._3)
        })
        assert(resultPGV.exists {
          case (tramo, progresiva, _) =>
            progresiva >= expectedResults("PGV")._1 &&
              progresiva <= expectedResults("PGV")._2 &&
              tramo.contains(expectedResults("PGV")._3)
        })
      }
  }
}
