package algorithms
import io.vmchura.vevial.PlanarGeometric.BasicEje.{ EmptySeqEjeElements, TSeqEjeElementsBase}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.PointUnitaryVector
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TDirection.{AnyDirection, Direction}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{CircleSegment, FaintElement, RectSegment}

case class BasicSectionBuilder(in: PointUnitaryVector,out: PointUnitaryVector,dataToMatch: Seq[ProgPointTangent]) extends BuilderFixedPoints {

  override protected def buildElements: Either[Seq[Exception], TSeqEjeElementsBase] = {

    lazy val defaultRect = {
      val f0 = FaintElement(in.point,in.point)
      val recta = RectSegment(in.point,out.point)
      val f1 = FaintElement(out.point,out.point)
      List(f0,recta,f1)
    }
    val e = (in.direction,out.direction) match {
      case (AnyDirection(),AnyDirection()) => defaultRect
      case (AnyDirection(),_: Direction) => defaultRect
      case (_: Direction, AnyDirection()) =>defaultRect
      case (_: Direction,_:Direction) =>
        LinearEquationsSolver.buildCircleTangent(in,out) match {
          case Some(x) =>
            val c = CircleSegment(x.originPoint,x.centerPoint,x.endPoint,x.antiClockWise)
            val left = if(c.originPoint ==? in.point) None else Some(RectSegment(in.point,c.originPoint))
            val right = if(c.endPoint ==? out.point) None else Some(RectSegment(c.endPoint,out.point))
            List(left,Some(c),right).flatten
          case None => defaultRect
        }
    }

    val inefficientEje: TSeqEjeElementsBase = e.foldLeft(EmptySeqEjeElements() :TSeqEjeElementsBase){case (prevSeq,newElement) => prevSeq.append(newElement)}
    Right(inefficientEje)

  }

}
