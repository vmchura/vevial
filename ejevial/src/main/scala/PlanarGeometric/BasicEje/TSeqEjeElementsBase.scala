package PlanarGeometric.BasicEje

import PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TDirection}
import PlanarGeometric.EjeElement.{ElementPoint, FaintElement, TEjeElement, TSimpleEjeElement}


trait TSeqEjeElementsBase extends TEjeElement{

  def append(o: TSeqEjeElementsBase): TSeqEjeElementsBase
  def append(t: TSimpleEjeElement): TSeqEjeElementsBase = append(TSeqEjeElementsBase(t))
}

case class EmptySeqEjeElements() extends TSeqEjeElementsBase {

  override val length: Double = 0
  override val in: PointUnitaryVector = PointUnitaryVector(Point(0,0),TDirection())
  override val out: PointUnitaryVector = PointUnitaryVector(Point(0,0),TDirection())

  override def append(o: TSeqEjeElementsBase): TSeqEjeElementsBase= o


  override def pointIsInsideElement(point: Point): Boolean = false



  override def lengthToPoint(point: ElementPoint): Double = 0

  override def projectPoint(point: Point): Option[ElementPoint] = None

  override val leftmostPoint: Point = Point(0,0)

  override val rightmostPoint: Point = Point(0,0)

  override val upperPoint: Point = Point(0,0)

  override val lowerPoint: Point = Point(0,0)
}

/**
  * Many methods not implement per efficiency, only append implemented
  * @param elements: SequenceOfElements
  */
case class NonEmptySeqEjeElements(elements: List[TSimpleEjeElement]) extends  TSeqEjeElementsBase {
  require(elements.nonEmpty)
  override val length: Double = elements.map(_.length).sum
  override val in: PointUnitaryVector = elements.head.in
  override val out: PointUnitaryVector = elements.last.out

  override def append(o: TSeqEjeElementsBase): TSeqEjeElementsBase = o match {
    case _ : EmptySeqEjeElements => this
    case t : NonEmptySeqEjeElements =>
      if( t.in ==? out)
        NonEmptySeqEjeElements(elements++t.elements)
      else
        NonEmptySeqEjeElements(elements++ Seq(FaintElement(elements.last.out.point,t.elements.head.in.point)) ++ t.elements)
    case _ => throw new IllegalStateException()
  }

  override def projectPoint(point: Point): Option[ElementPoint] = throw new NotImplementedError()

  override def pointIsInsideElement(point: Point): Boolean = throw new NotImplementedError()

  override def lengthToPoint(point: ElementPoint): Double = throw new NotImplementedError()

  override val leftmostPoint: Point = elements.map(_.leftmostPoint).minBy(_.x)

  override val  rightmostPoint: Point = elements.map(_.rightmostPoint).maxBy(_.x)

  override val  upperPoint: Point = elements.map(_.upperPoint).maxBy(_.y)

  override val  lowerPoint: Point = elements.map(_.lowerPoint).minBy(_.y)
}


object TSeqEjeElementsBase{




  /**
    * Per efficiency append not implemented
    * @param elements sequence of elements
    */


  def apply(elements: List[TSimpleEjeElement]): TSeqEjeElementsBase = NonEmptySeqEjeElements(elements)

  def apply(element: TSimpleEjeElement): TSeqEjeElementsBase = NonEmptySeqEjeElements(List(element))
  def apply(): TSeqEjeElementsBase= EmptySeqEjeElements()

}
