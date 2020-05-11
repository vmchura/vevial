package models.extensioneje

import io.vmchura.vevial.EjeVialBuilder.TConvertibleToEje
import io.vmchura.vevial.PlanarGeometric.BasicEje.{EfficientSeqEjeElements, EmptySeqEjeElements, TSeqEjeElementsBase}
import io.vmchura.vevial.PlanarGeometric.RestrictiveEje.ProgresivePoint
import models.TLinkPoint

class EjeFromLinks(headLink: TLinkPoint) extends TConvertibleToEje{
 // private val resultOfParsing: Either[Seq[Exception],(Seq[TSimpleEjeElement],Double)] = if(alignments.length == 1){

  private val elements = headLink.untilEnd().flatMap(_.elements)
  override protected def getSequenceElements: Either[Seq[Exception], EfficientSeqEjeElements] = {
    val inefficientEje = elements.foldLeft(EmptySeqEjeElements() :TSeqEjeElementsBase){case (prevSeq,newElement) => {

      prevSeq.append(newElement)
    }}

    Right(EfficientSeqEjeElements(inefficientEje))

  }

  override protected def getSequenceProgresivePoint: Iterable[ProgresivePoint] = Seq(new ProgresivePoint(elements.head.in.point,0d))
}
