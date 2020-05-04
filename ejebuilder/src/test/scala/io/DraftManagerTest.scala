package io

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TDirection}
import models.GeoLinkGraph
import org.scalatest.flatspec.AnyFlatSpec

class DraftManagerTest extends AnyFlatSpec {

  val l0 = new GeoLinkGraph(PointUnitaryVector(Point(0,0),TDirection(1,0)),
                            PointUnitaryVector(Point(1,-1),TDirection(0,-1)))
  val l1 = new GeoLinkGraph(PointUnitaryVector(Point(1,-1),TDirection(0,-1)),
                            PointUnitaryVector(Point(0,-2),TDirection(-1,0)))
  l0.next = Some(l1)
  l1.prev = Some(l0)
  val pp = new scala.xml.PrettyPrinter(24, 4)
  "Link 2 XML" should "print correctly" in {
    val e = DraftManager.linkAsXML(l0)


    val link = DraftManager.loadSimpleLink(e)
    assert(l0.in ==? link.in)
    assert(l0.out ==? link.out)

  }

  "Sequence Link 2 XML" should "save and load" in {
    val e = DraftManager.generateDraftAsXML(l0)
    val loaded = DraftManager.loadDraft(e)
    val List(d0,d1) = loaded.untilEnd()
    assert(l0.in ==? d0.in)
    assert(l0.out ==? d0.out)

    assert(l1.in ==? d1.in)
    assert(l1.out ==? d1.out)
  }
}
