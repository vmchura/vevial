package algorithms
import io.vmchura.vevial.PlanarGeometric.BasicEje.{EmptySeqEjeElements, TSeqEjeElementsBase}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.PointUnitaryVector
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TDirection.{AnyDirection, Direction}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{CircleSegment, RectSegment}

import scala.xml.Node


case class BasicSectionBuilder(in: PointUnitaryVector,out: PointUnitaryVector,dataToMatch: Seq[ProgPointTangent]) extends BuilderFixedPoints {


  override protected def buildElements: Either[Exception, TSeqEjeElementsBase] = {

    lazy val defaultRect = {

      val recta = RectSegment(in.point,out.point)

      List(recta)
    }
    val e = (in.direction,out.direction) match {
      case (AnyDirection(),AnyDirection()) => defaultRect
      case (AnyDirection(),_: Direction) => defaultRect
      case (_: Direction, AnyDirection()) =>defaultRect
      case (_: Direction,_:Direction) =>
        LinearEquationsSolver.buildCircleTangent(in,out) match {
          case Some(x) =>
            val c = CircleSegment(x.originPoint,x.centerPoint,x.endPoint,x.antiClockWise)
            val left = if(c.originPoint ==? in.point) None else Some(RectSegment(in.point,c.originPoint))
            val right = if(c.endPoint ==? out.point) None else Some(RectSegment(c.endPoint,out.point))
            List(left,Some(c),right).flatten
          case None => defaultRect
        }
    }

    val inefficientEje: TSeqEjeElementsBase = e.foldLeft(EmptySeqEjeElements() :TSeqEjeElementsBase){case (prevSeq,newElement) => prevSeq.append(newElement)}
    Right(inefficientEje)

  }

}

object BasicSectionBuilder{
  import scala.xml.Elem
  import io.ElementsAsXML._

  implicit class BasicSectionBuilderSerializable(val section: BasicSectionBuilder) extends SectionSerializable[BasicSectionBuilder] {

    override def saveToNodeXML(): Elem = {
      <SectionBuilder>
        <in>
        {savePointAsXML(section.in.point)}
        {saveDirectionAsXML(section.in.direction)}
        </in>
        <out>
          {savePointAsXML(section.out.point)}
          {saveDirectionAsXML(section.out.direction)}
        </out>
        <data>
          {section.dataToMatch.map(ProgPointTangent.saveAsXML)}
        </data>
      </SectionBuilder>
    }
  }

  def loadBasicSectionBuilderFromNodeXML(elem: Node): Option[BasicSectionBuilder] = {

      for{
        inNode <- (elem \ "in").headOption
        outNode <- (elem \ "out").headOption
      }yield{
        val in = loadPointUnitaryVector(inNode)
        val out = loadPointUnitaryVector(outNode)
        val data = (elem \ "data" \\ "ProgPointTangent").flatMap(ProgPointTangent.loadFromXML)
        BasicSectionBuilder(in,out,data)
      }

  }
  def saveSequenceBasicSectionBuilder(data: Seq[BasicSectionBuilder], path: String): Boolean = {
    val e = <DataBasicSectionBuilder>
      {
        data.map(_.saveToNodeXML())
      }
    </DataBasicSectionBuilder>
    try {
      scala.xml.XML.save(path,e)
      true
    }catch {
      case _: Throwable => false
    }
  }


}
