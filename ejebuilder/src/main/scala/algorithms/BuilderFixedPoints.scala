package algorithms

import io.vmchura.vevial.PlanarGeometric.BasicEje.TSeqEjeElementsBase
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.PointUnitaryVector
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TDirection.{AnyDirection, Direction}

trait BuilderFixedPoints {
  def in: PointUnitaryVector
  def out: PointUnitaryVector
  def dataToMatch: Seq[ProgPointTangent]
  protected def buildElements: Either[Seq[Exception],TSeqEjeElementsBase]
  final def elements: Either[Seq[Exception],TSeqEjeElementsBase] = {

    def almostSameElementAsInput(a: PointUnitaryVector, b: PointUnitaryVector): Boolean = {
      (a.point ==? b.point) &&
        ((a.direction,b.direction) match {
        case (d0: Direction, d1: Direction) =>d0 ==? d1
        case (_,_: AnyDirection) => true
        case _ => false
        })
    }

    buildElements match {
      case Left(errors) => Left(errors)
      case Right(base) =>
        if(almostSameElementAsInput(in,base.in) && almostSameElementAsInput(out,base.out)){
          Right(base)
        }else{
          Left(List(new IllegalStateException("In our out are not almost as to the parameters")))
        }
    }
  }
}
