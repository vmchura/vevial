package EjeVialBuilder
import PlanarGeometric.BasicEje.{EfficientSeqEjeElements, TSeqEjeElementsBase}
import PlanarGeometric.BasicGeometry.Point
import PlanarGeometric.EjeElement.{CircleSegment, RectSegment}
import PlanarGeometric.RestrictiveEje.ProgresivePoint

sealed class SimpleConvertibleToEje() extends TConvertibleToEje {

  override protected def getSequenceElements: Either[Seq[Exception], EfficientSeqEjeElements] = {
    val r = RectSegment(Point(100,0),Point(200,0))
    val c = CircleSegment(Point(200,0),Point(200,100),Point(200,200),antiClockWise = true)
    Right(EfficientSeqEjeElements(TSeqEjeElementsBase().append(r).append(c)))
  }

  override protected def getSequenceProgresivePoint: Iterable[ProgresivePoint] = List(new ProgresivePoint(Point(150,0),50),
    new ProgresivePoint(Point(200,0),100))
}
