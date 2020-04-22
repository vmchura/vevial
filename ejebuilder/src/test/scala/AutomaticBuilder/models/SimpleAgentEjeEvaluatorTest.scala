package AutomaticBuilder.models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TDirection}
import models.{GeoLinkGraph, ObserverImpl}
import org.scalatest.flatspec.AnyFlatSpec

class SimpleAgentEjeEvaluatorTest extends AnyFlatSpec {

  "SimpleAgentEjeEvaluator" should "give a razonable action" in {
    val gd = new GeoLinkGraph(PointUnitaryVector(Point(0,0),TDirection(1,0)),PointUnitaryVector(Point(20,-10),TDirection(0,-1d)))
    val observerD = new ObserverImpl(gd)

    observerD.addProjection(Point(5,5))
    observerD.addProjection(Point(6,6))
    object SimpleAgentEvaluatorImpl extends SimpleAgentEjeEvaluator
    val action = SimpleAgentEvaluatorImpl.deliberateAnAction(observerD)
    val res = action match {
      case NoAction => false
      case SetPointAt(x,y) if x>= 2 && x <= 10  && y>0 && y<6 =>  true
    }
    assert(res)
  }
  "SimpleAgentEjeEvaluator" should "give a razonable negative action" in {
    val gd = new GeoLinkGraph(PointUnitaryVector(Point(0,0),TDirection(1,0)),PointUnitaryVector(Point(20,-10),TDirection(0,-1d)))
    val observerD = new ObserverImpl(gd)

    observerD.addProjection(Point(5,-5))
    observerD.addProjection(Point(6,-6))
    object SimpleAgentEvaluatorImpl extends SimpleAgentEjeEvaluator
    val action = SimpleAgentEvaluatorImpl.deliberateAnAction(observerD)
    val res = action match {
      case NoAction => false
      case SetPointAt(x,y) if x>= 2 && x <= 10  && y>0 && y<6 =>  true
    }
    assert(res)
  }
}
