package PlanarGeometric

import PlanarGeometric.BasicEje.{EfficientSeqEjeElements, NonEmptySeqEjeElements, TSeqEjeElementsBase}
import PlanarGeometric.BasicGeometry.Point
import PlanarGeometric.EjeElement.{CircleSegment, FaintElement, RectSegment}
import org.scalatest.FlatSpec

class TSeqEjeElementsBaseTest extends FlatSpec {

  "Creation of Empty List" should " construct correctly with no error" in {
    //val r = RectSegment(Point(0,0),Point(1,0))
    //val c = CircleSegment(Point(1,0),Point(1,1),Point(1,2),antiClockWise = true)
    val sequence = TSeqEjeElementsBase()//.append(TSeqEjeElementsBase(r)).append(TSeqEjeElementsBase(c))

  }
  "Creation of Single Element list " should " construct correctly with no error"  in {
    val r = RectSegment(Point(0,0),Point(1,0))
    //val c = CircleSegment(Point(1,0),Point(1,1),Point(1,2),antiClockWise = true)
    val sequence = TSeqEjeElementsBase(r)

  }
  "Creation of sequence of elements" should " construct correctly with no error" in {
    val r = RectSegment(Point(0,0),Point(1,0))
    val c = CircleSegment(Point(1,0),Point(1,1),Point(1,2),antiClockWise = true)
    val sequence = TSeqEjeElementsBase().append(r).append(c)

  }
  "Creation of sequence of elements" should " construct correctly as expected" in {
    val r = RectSegment(Point(0,0),Point(1,0))
    val c = EjeElement.CircleSegment(Point(1,0),Point(1,1),Point(1,2),antiClockWise = true)
    val sequence = TSeqEjeElementsBase().append(r).append(c)
    assertResult(NonEmptySeqEjeElements(List(r,c)))(sequence)
  }
  "Creation of sequence incomplete of elements" should " construct correctly as expected" in {
    val r = RectSegment(Point(0,0),Point(1,0))
    val c = EjeElement.CircleSegment(Point(2,0),Point(2,1),Point(2,2),antiClockWise = true)
    val sequence = TSeqEjeElementsBase().append(r).append(c)
    assertResult(NonEmptySeqEjeElements(List(r,FaintElement(Point(1,0),Point(2,0)),c)))(sequence)
  }
  "Test of projections on a elemnts" should " should find correclty points" in {


    val c = EjeElement.CircleSegment(Point(1,0),Point(1,1),Point(1,2),antiClockWise = true)

    val y = Point(3,1)
    val projectionElementY = c projectPoint y
    assert(projectionElementY.isDefined)
    assertResult(Point(2,1))(projectionElementY.get.point)

  }

  "Test of projections on a series" should " should find correclty points" in {
    val r = RectSegment(Point(0,0),Point(1,0))
    val c = EjeElement.CircleSegment(Point(1,0),Point(1,1),Point(1,2),antiClockWise = true)
    val sequence = EfficientSeqEjeElements(TSeqEjeElementsBase().append(r).append(c))

    val x = Point(0.5,0.5)
    val projectionElement = sequence projectPoint x
    assert(projectionElement.isDefined)

    assertResult(Point(0.5,0.0))(projectionElement.get.point)

    val y = Point(3,1)
    val projectionElementY = sequence projectPoint y
    assert(projectionElementY.isDefined)


    assertResult(Point(2d,1d))(projectionElementY.get.point)

  }
  "Test of projections on an incomplete series" should " find correctly points" in {
    val r = RectSegment(Point(0,0),Point(1,0))
    val c = EjeElement.CircleSegment(Point(2,0),Point(2,1),Point(2,2),antiClockWise = true)
    val sequence = EfficientSeqEjeElements(TSeqEjeElementsBase().append(r).append(c))

    val x = Point(0.5,0.5)
    val projectionElement = sequence projectPoint x
    assert(projectionElement.isDefined)
    assertResult(Point(0.5,0))(projectionElement.get.point)

    val y = Point(4,1)
    val projectionElementY = sequence projectPoint y
    assert(projectionElementY.isDefined)
    assertResult(Point(3,1))(projectionElementY.get.point)
    assertResult(1+(Math.PI*1)/2.0)(sequence lengthToPoint projectionElementY.get)
  }
}
