package models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TDirection.Direction
import io.vmchura.vevial.elementdata.UPoint
import org.scalatest.flatspec.AnyFlatSpec

class PointTangentTest extends AnyFlatSpec {

  private val p0 = PointAlone(Some(UPoint(Point(0,0),0.5d)))
  private val p1 = PointAlone(Some(UPoint(Point(7.5,-2.5),0.5d)))
  private val p2 = PointAlone(Some(UPoint(Point(10d,-10d),0.5d)))

  private val q0 = PointAlone(Some(UPoint(Point(1,0),0.5d)))
  private val q1 = PointAlone(Some(UPoint(Point(8.5,-2.5),0.5d)))
  private val q2 = PointAlone(Some(UPoint(Point(11d,-10d),0.5d)))

  "Building" should "Produce correct type" in {
    val p = p1.withPrev(p0).withNext(p2)
    val q = p1.withNext(p2).withPrev(p0)
    assert(p.isInstanceOf[PointWithPrevNext])
    assert(q.isInstanceOf[PointWithPrevNext])
    assert(p === q)
  }
  "Building" should "Produce correct tangents" in {
    val p = p1.withPrev(p0).withNext(p2)
    val t = p.tangent
    assert(t.value.isInstanceOf[Direction])
    assert(t.value.dx > 0)
    assert(t.value.dy < 0)
  }
  "Building" should "Produce correct tangents reversed" in {
    val q = q1.withPrev(p2).withNext(p0)
    val t = q.tangent
    assert(t.value.isInstanceOf[Direction])
    assert(t.value.dx < 0)
    assert(t.value.dy > 0)
  }

  "Merging on tangents" should "Produce enhanced value" in {
    val p = p1.withPrev(p0).withNext(p2)
    val q = q1.withPrev(q0).withNext(q2)
    val t = p.tangent |-| q.tangent
    val m = for{
      r0 <- p.point
      r1 <- q.point
    }yield{
      r0 |-| r1
    }
    assert(m.isDefined)
    assert(m.get.value ==? Point(8.0d,-2.5d))
    assert(p.tangent.value ==? t.value)
    assert(t.sigma2 < p.tangent.sigma2)
  }

}
