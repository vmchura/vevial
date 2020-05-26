package algorithms

import java.io.File

import io.DraftManager
import io.vmchura.vevial.EjeVialBuilder.TConvertibleToEje
import io.vmchura.vevial.PlanarGeometric.BasicEje.{EfficientSeqEjeElements, EmptySeqEjeElements, TSeqEjeElementsBase}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{CircleSegment, FaintElement, RectSegment}
import io.vmchura.vevial.PlanarGeometric.RestrictiveEje.ProgresivePoint
import models.{CircleTemporal, FaintTemporal, RectTemporal}
import org.scalatest.flatspec.AnyFlatSpec

import scala.xml.{Elem, XML}

class AssignTangentsTest extends AnyFlatSpec {

  "Generation of eje" should "give a large eje" in {
    val file = new File("/home/vmchura/Documents/testProject.xml")
    val doc: Elem = XML.loadFile(file)
    val (linkInitial,seqFiles) = DraftManager.loadProject(doc)

    val convertibleEje: TConvertibleToEje = new TConvertibleToEje{
      private val links = linkInitial.untilEnd().flatMap(_.elements).map{
        case RectTemporal(originPoint, endPoint, _) => RectSegment(originPoint,endPoint)
        case CircleTemporal(originPoint, centerPoint, endPoint, antiClockWise, _) => CircleSegment(originPoint,centerPoint,endPoint,antiClockWise)
        case FaintTemporal(from, end, _) => FaintElement(from,end)
      }

      override protected def getSequenceElements: Either[Exception, EfficientSeqEjeElements] =
        try{
          val inefficientEje = links.foldLeft(EmptySeqEjeElements() :TSeqEjeElementsBase)((a,b) => a.append(b))
          Right(EfficientSeqEjeElements(inefficientEje))
        }catch {
          case e: Exception => Left(e)
          case t: Throwable => Left(new IllegalArgumentException(s"with the data cant produce EfficientSeqEjeElements : ${t.toString}"))
        }



      override protected def getSequenceProgresivePoint: Iterable[ProgresivePoint] = links.headOption match {
        case Some(x) => List(new ProgresivePoint(x.in.point,0d))
        case None => Nil
      }
    }

    val res: Either[Exception, TConvertibleToEje] = convertibleEje.toEje.map(efficientEjeProgresiva => {
      AssignTangents.calcEjeWithBasicMath(efficientEjeProgresiva,seqFiles)
    })


    res.flatMap(_.toEje) match {
      case Left(errors) => println(errors)
      case Right(z) => println(z.length)
    }

  }
}
