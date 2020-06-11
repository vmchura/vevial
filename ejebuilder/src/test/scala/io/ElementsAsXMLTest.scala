package io

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, TDirection}
import io.vmchura.vevial.elementdata.{UDirection, UPoint}
import org.scalatest.flatspec.AnyFlatSpec

class ElementsAsXMLTest extends AnyFlatSpec {

  behavior of "Save and load UPoint"
  it should "not throw errors" in {
    val u = UPoint(Point(1d,2d),3d)
    import ElementsAsXML._
    val element = saveUncertainElement(u,"point",savePointAsXML)
    val v = loadUncertainElement(element,"point",n => try Some(loadPoint(n)) catch {
      case _ : Throwable => None
    }, (d,e) => UPoint(e,d))
    assertResult(Some(u))(v)
  }
  behavior of "Save and load UDirection"
  it should "not throw errors" in {
    val u = UDirection(TDirection(1d,0d),5d)
    import ElementsAsXML._
    val element = saveUncertainElement(u,"direction",saveDirectionAsXML)
    val v = loadUncertainElement(element,"direction",n => try Some(loadDirection(n)) catch {
      case _ : Throwable => None
    }, (d,e) => UDirection(e,d))
    assertResult(Some(u))(v)
  }



}
