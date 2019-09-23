package EjeVialBuilder
import java.io.InputStreamReader
import scala.xml.{Node, XML}
import PlanarGeometric.BasicEje.{EfficientSeqEjeElements, EmptySeqEjeElements, TSeqEjeElementsBase}
import PlanarGeometric.BasicGeometry.Point
import PlanarGeometric.EjeElement.{CircleSegment, RectSegment, TSimpleEjeElement}
import PlanarGeometric.RestrictiveEje.ProgresivePoint
import com.typesafe.scalalogging.Logger


class LandXMLToEje(source: InputStreamReader) extends TConvertibleToEje {

  private val logger = Logger(classOf[LandXMLToEje])

  private val xml = XML.load(source)//XML.loadFile(filename)
  private val alignments = xml \\ "Alignment"
  private val resultOfParsing: Either[Seq[Exception],(Seq[TSimpleEjeElement],Double)] = if(alignments.length == 1){
    val aligment = alignments.head
    val attributes = Seq("desc","staStart","length","name")
    val values = attributes.map(k => aligment.attribute(k))
    if(values.forall(_.isDefined)) {
      val Seq(desc,staStart,length,name) = values.map(_.get.toString())
      logger.info("Parsing XML eje")
      logger.info("desc: "+desc)
      logger.info("staStart: "+staStart)
      logger.info("length: "+length)
      logger.info("name: "+name)

      val iniStart = (staStart+"0").toDouble
      val coordGeom = aligment \ "CoordGeom"
      val elementsNode = coordGeom.headOption.map(_.child).getOrElse(Nil)
      val elements = elementsNode.flatMap{
        case line @ <Line>{_*}</Line> => parseNode2Line(line)
        case curve @ <Curve>{_*}</Curve> => parseNode2Arc(curve)
        case _ => None
      }
      if(elements.isEmpty)
        Left(Seq(new IllegalArgumentException("0 elements to build eje")))
      else
        Right((elements.toSeq,iniStart))

    }else{
      Left(Seq(new IllegalStateException(s"Not all values defined [for ${attributes.mkString(",")}]")))

    }
  }else{
    Left(Seq(new IllegalStateException(s"wrong allignments.lengh!=1 [=${alignments.length}]")))
  }



  private def str2Point(str: String): Point = {
    val Array(norte,este) = str.split(" ").map(_.toDouble)
    Point(este,norte)
  }

  private def parseNode2Line(node: Node): Option[TSimpleEjeElement] = {
    //val startOpt =
    //val endOpt = (node \ "End").headOption
    for{
      start <- (node \ "Start").headOption
      end <- (node \ "End").headOption
    }yield{
      val pinicio = str2Point(start.text)
      val pfin = str2Point(end.text)
      RectSegment(pinicio,pfin)
    }

  }
  private def parseNode2Arc(node: Node): Option[TSimpleEjeElement] = {
    for{
      start <- (node \ "Start").headOption
      end <- (node \ "End").headOption
      center <- (node \ "Center").headOption
      rot <- node.attribute("rot").flatMap(_.headOption)
    }yield{
      val pinicio = str2Point(start.text)
      val pfin = str2Point(end.text)
      val pcenter= str2Point(center.text)
      val ccw = rot.text.trim.toLowerCase().equals("ccw")
      CircleSegment(pinicio,pcenter,pfin,antiClockWise = ccw)
    }





  }

  override protected def getSequenceElements: Either[Seq[Exception], EfficientSeqEjeElements] = resultOfParsing.map {
    case (elements,_)=>
      val inefficientEje = elements.foldLeft(EmptySeqEjeElements() :TSeqEjeElementsBase){case (prevSeq,newElement) => prevSeq.append(newElement)}

      EfficientSeqEjeElements(inefficientEje)
  }

  override protected def getSequenceProgresivePoint: Iterable[ProgresivePoint] = resultOfParsing match {
    case Right((h :: _,start)) => Seq(new ProgresivePoint(h.in.point,start))
    case _ => Nil
  }
}
