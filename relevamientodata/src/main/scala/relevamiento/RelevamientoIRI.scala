package relevamiento
import java.io.InputStream
import elementdata.{CrudeIRIData, IRIElementData}

class RelevamientoIRI(source: InputStream) extends TSimpleRelevamiento[IRIElementData] {
  private val lines = scala.io.Source.fromInputStream(source).getLines()
  private val registers = lines.zipWithIndex.dropWhile{case (_,indx) => indx <25}.map{case (line,_) => line}
  private val dataElements = registers.map(line => IRIElementData(new CrudeIRIData(line))).toArray
  private val n = dataElements.length
  dataElements.zipWithIndex.foreach{
    case (iriElement, indx) =>

      if(indx>0)   dataElements.update(indx,iriElement.withPrevElement(dataElements(indx-1)))
      if(indx+1<n) dataElements.update(indx,iriElement.withNextElement(dataElements(indx+1)))

  }

  override def elements: Seq[IRIElementData] = dataElements
}
