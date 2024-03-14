package io.vmchura.vevial.EjeVialBuilder

import io.vmchura.vevial.EjeVialUtil.Coordinates
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Codec
import scala.reflect.io.File

class PGVMultipleEjeProjectoSingularTest extends AnyFlatSpec with Matchers{

  val eje = new PGVMultipleEjeProjectoSingular()
  behavior of "ODNAMultipleEjeProjectoSingular"
  it should "build correctly" in {
    assert(eje.findProgresiva(Coordinates(latitud= -14.004850, longitud= -72.797108).toPoint()).exists{
      case (tramo, progresiva, _) =>
        println(tramo)
        progresiva >= 73900 && progresiva <= 74000
    })
    assert(eje.findProgresiva(Coordinates(latitud = -14.874001, longitud = -70.659430).toPoint()).exists {
      case (tramo, progresiva, _) =>
        println(tramo)
        progresiva >= 352800 && progresiva <= 352900
    })
  }
}
