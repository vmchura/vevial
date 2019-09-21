package relevamiento
import java.io.File

import elementdata.{CrudeIRIData, IRIElementData}



class RelevamientoIRI(source: File) extends TSimpleRelevamiento[IRIElementData] {
  private val lines = scala.io.Source.fromFile(source,"UTF-8")
  private val registers = lines.getLines().zipWithIndex.dropWhile{case (_,indx) => indx <25}.map{case (line,_) => line}


  private val dataElements = registers.map(line => IRIElementData(new CrudeIRIData(line))).toArray
  private val n = dataElements.length
  dataElements.zipWithIndex.foreach{
    case (iriElement, indx) =>

      if(indx>0)   dataElements.update(indx,iriElement.withPrevElement(dataElements(indx-1)))
      if(indx+1<n) dataElements.update(indx,iriElement.withNextElement(dataElements(indx+1)))

  }
  lines.close()
  override def elements: Seq[IRIElementData] = dataElements
}
