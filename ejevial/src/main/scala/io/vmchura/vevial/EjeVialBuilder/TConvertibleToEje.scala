package io.vmchura.vevial.EjeVialBuilder

import io.vmchura.vevial.PlanarGeometric.BasicEje.{EfficientSeqEjeElements, TEfficientSeqEjeElements}
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import io.vmchura.vevial.PlanarGeometric.RestrictiveEje.{EjeEfficientWithRestrictions, ProgresivePoint}

trait TConvertibleToEje {
  final def toEje: Either[Seq[Exception],EfficientEjeProgresiva] = {

    getSequenceElements.flatMap{ efficientSeqElements =>
        val sequenceWithRestrictions = EjeEfficientWithRestrictions(efficientSeqElements)
        val pointsProgresiveEither =
          getSequenceProgresivePoint.foldLeft(Right(sequenceWithRestrictions) : Either[Seq[Exception], EjeEfficientWithRestrictions[TEfficientSeqEjeElements, TEfficientSeqEjeElements]]){
            case (Left(error),_) => Left(error)
            case (Right(sr), pp) =>

              sr.addRestriction(pp) match {
                case Right(value) => Right(value)
                case Left(_) => Left(List(new IllegalStateException(s"Cant add $pp to $sr")))
              }
          }
        pointsProgresiveEither.map{ pointsProgresive =>
          EfficientEjeProgresiva(pointsProgresive)
        }

      }
    }


  protected def getSequenceElements: Either[Seq[Exception],EfficientSeqEjeElements]
  protected def getSequenceProgresivePoint: Iterable[ProgresivePoint]
}
