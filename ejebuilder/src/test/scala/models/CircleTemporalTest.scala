package models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point
import org.scalatest.flatspec.AnyFlatSpec

class CircleTemporalTest extends AnyFlatSpec {

  "Projection over circle" should "return positive projection" in {
    val radius = 10d
    val r = CircleTemporal(Point(radius,2*radius),Point(radius,radius),Point(2*radius,radius),antiClockWise = false,null)

    val testPoint = Point(2*radius,2*radius)
    r.projectionOverElement(testPoint) match {
      case Some(x) => {
        println(s"projection: $x")
        assert(x.distanceNormal>0)
        r.pointFromProjection(x) match {
          case Some(recovered) =>
            println(recovered)
            println(testPoint)
            assert(recovered ==? testPoint)
          case None =>
            throw new IllegalStateException("recovered point is not defined")
        }

      }
      case None => throw new IllegalStateException("projection it should be defined")
    }

  }
}
