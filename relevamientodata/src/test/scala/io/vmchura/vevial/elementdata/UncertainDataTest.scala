package io.vmchura.vevial.elementdata

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{PlanarVector, Point, TDirection}
import org.scalatest.flatspec.AnyFlatSpec

class UncertainDataTest extends AnyFlatSpec {

  val p = UPoint(Point(0,0),0.2d)
  val q = UPoint(Point(1,1),0.2d)
  val r = UPoint(Point(100,100),100d)

  val up = UPlanarVector(PlanarVector(TDirection(1,0),1),0.2d)
  val uq = UPlanarVector(PlanarVector(TDirection(0,1),1),0.2d)
  val ur = UPlanarVector(PlanarVector(TDirection(1,1),100),100d)


  "Middle point" should "give aproximate values" in {
    val m = p |-| q
    assertResult(Point(0.5d,0.5d))(m.value)
    assert(Math.abs(m.sigma2-0.1d)<1e-6)
  }
  "Middle point with uncertain point" should "give close to the other one" in {
    val m = p |-| r
    assert(m.value.x < 0.5)
    assert(m.value.x > 0d)
    assert(m.value.y < 0.5)
    assert(m.value.y > 0d)
    val n = q |-| r
    assert(n.value.x < 1.5)
    assert(n.value.x > 1d)
    assert(n.value.y < 1.5)
    assert(n.value.y > 1d)
  }
  "Middle Direction" should "give aproximate values" in {
    val m = UDirection(TDirection(1,0),0.2) |-| UDirection(TDirection(0,1),0.2)
    println(m)
  }
  "Middle vector" should "give aproximate values" in {
    val m = up |-| uq
    val dir = m.value
    val l = Math.sqrt(2d)/2d
    assert(dir ==? PlanarVector(TDirection(l,l),1))
  }
  "Middle vector with uncertain point" should "give close to the other one" in {
    val m = up |-| ur
    assert(m.value.direction.dx >= 0.9 && m.value.direction.dx < 1.1)
    assert(m.value.direction.dy >= -0.1 && m.value.direction.dy < 0.1)
  }

}
