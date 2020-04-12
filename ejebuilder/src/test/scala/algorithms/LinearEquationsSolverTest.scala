package algorithms

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TDirection}
import io.vmchura.vevial.PlanarGeometric.EjeElement.CircleSegment
import io.vmchura.vevial.elementdata.UDirection
import org.scalatest.flatspec.AnyFlatSpec

class LinearEquationsSolverTest extends AnyFlatSpec {

  behavior of "Tangent circle"
  it should "work on perpendicular cases" in {
    val a = PointUnitaryVector(Point(0,0),TDirection(1,0))
    val b = PointUnitaryVector(Point(10,-10),TDirection(0,-1))

    val res = LinearEquationsSolver.buildCircleTangent(a,b)
    assert(res.nonEmpty)
    val cs = res.get
    val csExpected = CircleSegment(Point(0.0,0.0),Point(0.0,-10.0),Point(10.0,-10.0),false)
    assert(cs.in ==? csExpected.in)
    assert(cs.out ==? csExpected.out)


  }
  it should "work on perpendicular cases trim top" in {
    val a = PointUnitaryVector(Point(1,0),TDirection(1,0))
    val b = PointUnitaryVector(Point(10,-10),TDirection(0,-1))

    val res = LinearEquationsSolver.buildCircleTangent(a,b)
    assert(res.nonEmpty)
    val cs = res.get
    val csExpected = CircleSegment(Point(1.0,0.0),Point(1.0,-9.0),Point(10.0,-9.0),false)
    assert(cs.in ==? csExpected.in)
    assert(cs.out ==? csExpected.out)
  }
  it should "work on perpendicular cases trim right" in {
    val a = PointUnitaryVector(Point(0,0),TDirection(1,0))
    val b = PointUnitaryVector(Point(10,-9),TDirection(0,-1))

    val res = LinearEquationsSolver.buildCircleTangent(a,b)
    assert(res.nonEmpty)
    val cs = res.get
    val csExpected = CircleSegment(Point(1.0,0.0),Point(1.0,-9.0),Point(10.0,-9.0),false)
    assert(cs.in ==? csExpected.in)
    assert(cs.out ==? csExpected.out)
  }

  it should "work on perpendicular cases reverse" in {
    val a = PointUnitaryVector(Point(0,0),TDirection(1,0))
    val b = PointUnitaryVector(Point(10,10),TDirection(0,1))

    val res = LinearEquationsSolver.buildCircleTangent(a,b)
    assert(res.nonEmpty)
    val cs = res.get
    val csExpected = CircleSegment(Point(0.0,0.0),Point(0.0,10.0),Point(10.0,10.0),true)
    assert(cs.in ==? csExpected.in)
    assert(cs.out ==? csExpected.out)


  }

  it should "give correct direction of tangent" in {
    val in =  PointUnitaryVector(Point(0,0),TDirection(0.759332296791468,0.6507030528969372))
    val out = PointUnitaryVector(Point(34.3754361718,29.972300179),TDirection(0.7667458829240443,0.6419507387790965))
    val res = LinearEquationsSolver.buildCircleTangent(in,out)
    assertResult(None)(res)
    //      [info] res: Some(CircleSegment(Point(584322.8009468049,8554201.76655548),Point(580457.7393970637,8558712.066685071),Point(584270.8137421691,8554157.731159337),true))
  }
}
