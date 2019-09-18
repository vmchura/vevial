package PlanarGeometric.ProgresiveEje

import PlanarGeometric.BasicEje.{EfficientSeqEjeElements, TEfficientSeqEjeElements, TSeqEjeElementsBase}
import PlanarGeometric.BasicGeometry.Point
import PlanarGeometric.EjeElement
import PlanarGeometric.EjeElement.{CircleSegment, RectSegment, TEjeElement}
import PlanarGeometric.RestrictiveEje.{EjeEfficientWithRestrictions, ProgresivePoint, WithRestrictionsIncremental}
import org.scalatest.{FlatSpec, Matchers}

class EfficientEjeProgresivaTest extends FlatSpec with Matchers{

  val pointLinearTests = List(new ProgresivePoint(Point(0.5,0),0.5),new ProgresivePoint(Point(2,1),1+Math.PI/2.0))
  val pointAlterTest = List(new ProgresivePoint(Point(0.5,0),1),new ProgresivePoint(Point(2,1),5))

  "Creation of restriction eje" should " build each element correctly " in {

    val r = RectSegment(Point(0,0),Point(1,0))

    val c = CircleSegment(Point(1,0),Point(1,1),Point(1,2),antiClockWise = true)

    val sequence = EfficientSeqEjeElements(TSeqEjeElementsBase().append(r).append(c))


    val sequenceWithRestrictions = EjeEfficientWithRestrictions(sequence)

    val pointsProgresive = pointLinearTests.foldLeft(sequenceWithRestrictions){case (sr, pp) => sr.addRestriction(pp) match {
      case Right(value) => value
      case Left(value) => value
    }}

    assertResult(2)(pointsProgresive.elementsWithRestrictions.length)



    val eje = EfficientEjeProgresiva(pointsProgresive)

    val ep0 = eje projectPoint Point(0,0)
    assert(ep0.isDefined)
    val prog0 = eje calcProgresive ep0.get
    assert(Math.abs(prog0-0.0)<1e-3)

    val ep1 = eje projectPoint Point(2,1)
    assert(ep1.isDefined)
    val prog1 = eje calcProgresive ep1.get
    assert(Math.abs(prog1-(1+Math.PI/2.0))<1e-3)

  }

  "Creation of restriction eje altered" should " build each element correctly " in {

    val r = RectSegment(Point(0,0),Point(1,0))
    val c = CircleSegment(Point(1,0),Point(1,1),Point(1,2),antiClockWise = true)
    val sequence = EfficientSeqEjeElements(TSeqEjeElementsBase().append(r).append(c))
    val sequenceWithRestrictions = EjeEfficientWithRestrictions(sequence)
    val pointsProgresive = pointAlterTest.foldLeft(sequenceWithRestrictions){case (sr, pp) => sr.addRestriction(pp) match {
      case Right(value) => value
      case Left(value) => value
    }}

    assertResult(2)(pointsProgresive.elementsWithRestrictions.length)



    val eje = EfficientEjeProgresiva(pointsProgresive)
    val ep0 = eje projectPoint Point(0,0)
    assert(ep0.isDefined)
    val prog0 = eje calcProgresive ep0.get
    assert(Math.abs(prog0-0.5)<1e-3)
    val ep1 = eje projectPoint Point(2,1)
    assert(ep1.isDefined)
    val prog1 = eje calcProgresive ep1.get
    assert(Math.abs(prog1-5)<1e-3)
    val ep2 = eje projectPoint Point(1,2)
    assert(ep2.isDefined)
    val prog2 = eje calcProgresive ep2.get
    assert(Math.abs(prog2-(5+Math.PI/2.0))<1e-3)
    val ep3 = eje projectPoint Point(1,0)
    assert(ep3.isDefined)
    val prog3 = eje calcProgresive ep3.get
    assert(Math.abs(prog3-(2/(Math.PI/2+0.5)+1))<1e-3)

  }

  "Calc of points by progresiva" should " find correctly " in {

    val pointLargerRestricctions = List(new ProgresivePoint(Point(50,0),50),new ProgresivePoint(Point(200,100),100+100*Math.PI/2.0))

    val r = RectSegment(Point(0,0),Point(100,0))

    val c = CircleSegment(Point(100,0),Point(100,100),Point(100,200),antiClockWise = true)

    val sequence = EfficientSeqEjeElements(TSeqEjeElementsBase().append(r).append(c))


    val sequenceWithRestrictions = EjeEfficientWithRestrictions(sequence)

    val pointsProgresive = pointLargerRestricctions.foldLeft(sequenceWithRestrictions){case (sr, pp) => sr.addRestriction(pp) match {
      case Right(value) => value
      case Left(value) => value
    }}

    assertResult(2)(pointsProgresive.elementsWithRestrictions.length)



    val eje = EfficientEjeProgresiva(pointsProgresive)

    val unoOpt = eje.findPointByProgresive(100)
    assert(unoOpt.isDefined)
    val Point(x1,y1) = unoOpt.get
    x1 should equal (100.0 +- 1e-3)
    y1 should equal (0.0 +- 1e-3)

    val dosOpt = eje.findPointByProgresive(150)
    assert(dosOpt.isDefined)
    val Point(x2,y2) = dosOpt.get
    x2 should equal (100.0+ Math.sin(0.5)*100 +- 1e-3)
    y2 should equal (100.0-Math.cos(0.5)*100 +- 1e-3)
  }

}
