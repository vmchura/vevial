package algorithms

import io.vmchura.vevial.PlanarGeometric.BasicEje.TSeqEjeElementsBase
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.PointUnitaryVector

trait BuilderFixedPoints {
  def in: PointUnitaryVector
  def out: PointUnitaryVector
  def dataToMatch: Seq[ProgPointTangent]
  protected def buildElements: Either[Seq[Exception],TSeqEjeElementsBase]
  final def elements: Either[Seq[Exception],TSeqEjeElementsBase] = {

    def almostSameElementAsInput(a: PointUnitaryVector, b: PointUnitaryVector): Boolean = a.point ==? b.point


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
