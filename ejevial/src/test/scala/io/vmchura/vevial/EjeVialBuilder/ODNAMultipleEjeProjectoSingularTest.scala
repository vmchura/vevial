package io.vmchura.vevial.EjeVialBuilder

import io.vmchura.vevial.EjeVialUtil.Coordinates
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Codec
import scala.reflect.io.File

class ODNAMultipleEjeProjectoSingularTest extends AnyFlatSpec with Matchers{

  val eje = new ODNAMultipleEjeProjectoSingular()
  behavior of "ODNAMultipleEjeProjectoSingular"
  it should "build correctly" in {
    assert(eje.findProgresiva(Coordinates(latitud= -14.004850, longitud= -72.797108).toPoint()).exists{
      case (tramo, progresiva, _) =>
        progresiva >= 73800 && progresiva <= 73900 && tramo.contains("Tramo I")
    })
  }
}
