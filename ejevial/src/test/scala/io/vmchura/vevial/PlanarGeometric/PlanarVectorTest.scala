package io.vmchura.vevial.PlanarGeometric

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{TDirection, PlanarVector, ZVector}
import org.scalatest.FlatSpec

class PlanarVectorTest extends FlatSpec {
  val v = TDirection(1,0)
  val u = TDirection(0,1)
  val f1 = PlanarVector(v,3)
  val f2 = PlanarVector(u,4)
  "A Unitary Vector " should " rotate correctly " in {

    val v1 = v <¬ 1
    val v2 = v <¬ 2
    val v3 = v <¬ 3
    val v4 = v <¬ 4
    assertResult(TDirection(0,1))(v1)
    assertResult(TDirection(-1,0))(v2)
    assertResult(TDirection(0,-1))(v3)
    assertResult(TDirection(1,0))(v4)

  }
  "A Vector " should " add correctly " in {


    val g = f1 + f2
    assert(Math.abs(g.magnitude-5)<1e-5)
    assert(Math.abs(!g-5)<1e-5)

  }
  "A Vector " should " implement dot and cross product" in {

    val dotProduct = f1 * f2
    val crossProductPositive = f1 x f2
    val crossProductNegative = f2 x f1
    assert(Math.abs(dotProduct-0.0)<1e-5)
    assert(crossProductPositive == ZVector(12))
    assert(crossProductNegative == ZVector(-12))

  }
  "A Vector " should " implement positive angle to vector" in {

    val pi_2 = f1 \/ f2
    val pi3_2 = f2 \/ f1
    assert(Math.abs(pi_2-Math.PI/2)<1e-5)
    assert(Math.abs(pi3_2-3*Math.PI/2)<1e-5)

  }
  "A Vector " should " implement equivalent to another vector" in {

    val f3 = PlanarVector(v,3+1e-6)

    assert(f1 ==? f3)

  }
  "A Vector " should " implement rotation to left for an arbitrary angle" in {

    val f3 = f1 << Math.PI/2.0
    val f4 = f1 <¬ 1
    assert(f3 ==? f4)

  }
}
