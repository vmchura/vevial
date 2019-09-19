package EjeVialBuilder
import java.io.InputStreamReader

import scala.xml.{Node, XML}
import PlanarGeometric.BasicEje.EfficientSeqEjeElements
import PlanarGeometric.EjeElement.TSimpleEjeElement
import PlanarGeometric.RestrictiveEje.ProgresivePoint
import com.typesafe.scalalogging.Logger

import scala.collection.mutable

class LandXMLToEje(source: InputStreamReader) extends TConvertibleToEje {
  val elements = new mutable.ListBuffer[TSimpleEjeElement]()
  val logger = Logger(classOf[LandXMLToEje])

  val xml = XML.load(source)//XML.loadFile(filename)
  val allignments = xml \\ "Alignment"
  if(allignments.length == 1){

  }else{
    logger.error(s"wrong allignments.lengh!=1 [=${allignments.length}]")
  }

  override protected def getSequenceElements(): EfficientSeqEjeElements = ???

  override protected def getSequenceProgresivePoint(): Iterable[ProgresivePoint] = ???
}
