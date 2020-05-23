package algorithms

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point
import io.vmchura.vevial.elementdata.{IRIElementData, UPoint}
import io.vmchura.vevial.relevamiento.RelevamientoIRI
import models.GeoNode
import org.scalatest.flatspec.AnyFlatSpec

class DiscreteRelevamientoTest extends AnyFlatSpec {

  behavior of "Discrete relevamiento"

  it should "create correct results" in {
    val simpleRelevamiento = RelevamientoIRI(Seq(
      IRIElementData(Some(UPoint(Point(0,0),0d)),None,None,None,None),
      IRIElementData(Some(UPoint(Point(5,0),0d)),None,None,None,None),
      IRIElementData(Some(UPoint(Point(10,0),0d)),None,None,None,None),
      IRIElementData(Some(UPoint(Point(15,0),0d)),None,None,None,None),
      IRIElementData(Some(UPoint(Point(20,0),0d)),None,None,None,None),
      IRIElementData(Some(UPoint(Point(25,0),0d)),None,None,None,None),
      IRIElementData(Some(UPoint(Point(30,0),0d)),None,None,None,None),
      IRIElementData(Some(UPoint(Point(35,0),0d)),None,None,None,None),
      IRIElementData(Some(UPoint(Point(40,0),0d)),None,None,None,None)
    )
      ,Nil,0)

    val bosquejoEje = DiscreteRelevamiento.convertIntoDiscreteRelevamiento[RelevamientoIRI[IRIElementData],IRIElementData,GeoNode](List(simpleRelevamiento))

    assertResult(1)(bosquejoEje.length)
    assertResult(3)(bosquejoEje.head.nodes.length)
  }
}
