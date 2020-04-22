package AutomaticBuilder.models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TDirection}
import models.{GeoLinkGraph, ObserverImpl}
import org.scalatest.flatspec.AnyFlatSpec

class TElementCanImproveTest extends AnyFlatSpec {
    "TElementCanImproveTest" should "give correct results over the rect" in {
      val gd = new GeoLinkGraph(PointUnitaryVector(Point(0,0),TDirection(1,0)),PointUnitaryVector(Point(20,-10),TDirection(0,-1d)))
      val gr = new GeoLinkGraph(PointUnitaryVector(Point(20,-10),TDirection(0,1d)),PointUnitaryVector(Point(0,0),TDirection(-1d,0d)))
      val observerD = new ObserverImpl(gd)
      val observerR = new ObserverImpl(gr)
      val point = Point(5,5)
      observerD.addProjection(point)
      observerR.addProjection(point)
      assert(Math.abs(List(observerD,observerR).map(_.elementsAdded().head.distanceNormal).sum) < 1e-6)
      assert(Math.abs(List(observerD,observerR).map(_.elementsAdded().head.distanceOverElement).sum - gd.length) < 1e-6)


    }
  "TElementCanImproveTest" should "give correct reverse over circ" in {
    val gd = new GeoLinkGraph(PointUnitaryVector(Point(0,0),TDirection(1,0)),PointUnitaryVector(Point(20,-10),TDirection(0,-1d)))
    val gr = new GeoLinkGraph(PointUnitaryVector(Point(20,-10),TDirection(0,1d)),PointUnitaryVector(Point(0,0),TDirection(-1d,0d)))
    val observerD = new ObserverImpl(gd)
    val observerR = new ObserverImpl(gr)
    val point = Point(20,0)
    observerD.addProjection(point)
    observerR.addProjection(point)
    assert(Math.abs(List(observerD,observerR).map(_.elementsAdded().head.distanceNormal).sum) < 1e-6)
    assert(Math.abs(List(observerD,observerR).map(_.elementsAdded().head.distanceOverElement).sum - gd.length) < 1e-6)
  }
}
