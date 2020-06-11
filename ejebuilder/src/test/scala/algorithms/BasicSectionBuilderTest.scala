package algorithms

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TDirection}
import io.vmchura.vevial.elementdata.{UDirection, UPoint}
import models.PointTangentDefined
import org.scalatest.flatspec.AnyFlatSpec

class BasicSectionBuilderTest extends AnyFlatSpec {

  behavior of "BasicSectionBuilder"
  it should "Save and load correctly as XML" in {
    val in  = PointUnitaryVector(Point(10d,10d), TDirection(1,0))
    val out = PointUnitaryVector(Point(5d,5d), TDirection(0,1))
    val data = List(
      ProgPointTangent(10,PointTangentDefined(Some(UPoint(Point(1,1),5d)),UDirection(TDirection(-1,0),1d))),
      ProgPointTangent(20,PointTangentDefined(Some(UPoint(Point(2,2),5d)),UDirection(TDirection(0,-1),2d))),
      ProgPointTangent(30,PointTangentDefined(Some(UPoint(Point(3,3),5d)),UDirection(TDirection(1, 0),3d)))
    )
    val basicSectionBuilder = BasicSectionBuilder(in,out,data)
    import BasicSectionBuilder._
    val element = basicSectionBuilder.saveToNodeXML()
    val bb = loadBasicSectionBuilderFromNodeXML(element)
    assertResult(Some(basicSectionBuilder))(bb)
  }

}
