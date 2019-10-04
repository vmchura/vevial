package relevamiento
import java.io.File

import elementdata.{CrudeIRIData, IRIElementData, UPoint}



class RelevamientoIRI(override val elements: Seq[IRIElementData]) extends TSimpleRelevamiento[IRIElementData] {

  override def sliceBy(minX: Double, maxX: Double, minY: Double, maxY: Double): RelevamientoIRI = {
    new RelevamientoIRI(elements.filter{ e =>
      e.point.exists { case UPoint(value, _) =>
        minX <= value.x && value.x <= maxX &&
          minY <= value.y && value.y <= maxY
      }
    })
  }
}

object RelevamientoIRI{
  def apply(elements: Seq[IRIElementData]): RelevamientoIRI = new RelevamientoIRI(elements)

  def apply(source: File): RelevamientoIRI = {
   val lines = scala.io.Source.fromFile(source,"UTF-8")
   val registers = lines.getLines().zipWithIndex.dropWhile{case (_,indx) => indx <25}.map{case (line,_) => line}
   val dataElements = registers.map(line => IRIElementData(new CrudeIRIData(line))).toArray
   val n = dataElements.length
   dataElements.zipWithIndex.foreach{
   case (iriElement, indx) =>

     if(indx>0)   dataElements.update(indx,iriElement.withPrevElement(dataElements(indx-1)))
     if(indx+1<n) dataElements.update(indx,iriElement.withNextElement(dataElements(indx+1)))

   }
   lines.close()
    new RelevamientoIRI(dataElements)

  }
}

