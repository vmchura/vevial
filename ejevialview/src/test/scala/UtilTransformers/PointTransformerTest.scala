package UtilTransformers

import Layers.EjeVialLayer
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point
import io.vmchura.vevial.PlanarGeometric.EjeElement.RectSegment
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.WithDistributionFormula
import io.vmchura.vevial.PlanarGeometric.RestrictiveEje.{ProgresivePoint, RectSegmentRestrictions}

import org.scalatest.{FlatSpec, Matchers}
import scalafx.scene.shape.Line

class PointTransformerTest extends FlatSpec with Matchers{

  "Line change" should "change correctly values" in {

    import WithDistributionFormula._
    val r = RectSegment(Point(200, 0), Point(300, 0))
    val rr =
      (for {
        w1 <- RectSegmentRestrictions(r, Nil).addRestriction(new ProgresivePoint(Point(200, 0), 0))
        w2 <- w1.addRestriction(new ProgresivePoint(Point(300, 0), 100))
      } yield {

        w2
      }) match {
        case Left(value) => value
        case Right(value) => value
      }
    val rp = rr match {
      case rwtt: RectSegmentRestrictions => WithDistributionFormula.convert(rwtt)
      case _ => throw  new IllegalStateException()
    }

    val shapeLine = EjeVialLayer.elementConversor(rp).head.asInstanceOf[Line]

    import PointTransformer._

    shapeLine.startX() should equal (200.0 +- 0.1 )
    shapeLine.endX() should equal (300.0 +- 0.1 )

    updateOffsetWithPivot(10.0,0.0,0)

    shapeLine.startX() should equal (20.0 +- 0.1 )
    shapeLine.endX() should equal (30.0 +- 0.1 )

    offsetX() = 50.0

    shapeLine.startX() should equal (15.0 +- 0.1 )
    shapeLine.endX() should equal (25.0 +- 0.1 )

    updateOffsetWithPivot(20.0,100.0,0)

    shapeLine.startX() should equal (10.0 +- 0.1 )
    shapeLine.endX() should equal (15.0 +- 0.1 )

  }
}
