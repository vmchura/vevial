package io.vmchura.vevial.EjeVialBuilder

import io.vmchura.vevial.PlanarGeometric.BasicEje.{EfficientSeqEjeElements, TEfficientSeqEjeElements}
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import io.vmchura.vevial.PlanarGeometric.RestrictiveEje.{EjeEfficientWithRestrictions, ProgresivePoint}

trait TConvertibleToEje {
  final def toEje: Either[Exception,EfficientEjeProgresiva] = {

    getSequenceElements.flatMap{ efficientSeqElements =>
        val sequenceWithRestrictions = EjeEfficientWithRestrictions(efficientSeqElements)
        val pointsProgresiveEither =
          getSequenceProgresivePoint.foldLeft(Right(sequenceWithRestrictions) : Either[Exception, EjeEfficientWithRestrictions[TEfficientSeqEjeElements, TEfficientSeqEjeElements]]){
            case (Left(error),_) => Left(error)
            case (Right(sr), pp) =>

              sr.addRestriction(pp) match {
                case Right(value) => Right(value)
                case Left(value) =>
                  println(s"Cant add $pp - ${pp.progresive}")
                  Right(value)

              }
          }
        pointsProgresiveEither.map{ pointsProgresive =>
          EfficientEjeProgresiva(pointsProgresive)
        }

      }
    }


  protected def getSequenceElements: Either[Exception,EfficientSeqEjeElements]
  protected def getSequenceProgresivePoint: Iterable[ProgresivePoint]
}
