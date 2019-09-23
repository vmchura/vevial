package EjeVialBuilder

import PlanarGeometric.BasicEje.EfficientSeqEjeElements
import PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import PlanarGeometric.RestrictiveEje.{EjeEfficientWithRestrictions, ProgresivePoint}
import com.typesafe.scalalogging.Logger

trait TConvertibleToEje {
  final def toEje: Either[Seq[Exception],EfficientEjeProgresiva] = {


    getSequenceElements.map{ efficientSeqElements =>

        val sequenceWithRestrictions = EjeEfficientWithRestrictions(efficientSeqElements)
        val logger = Logger(classOf[TConvertibleToEje])

        val pointsProgresive =
          getSequenceProgresivePoint.foldLeft(sequenceWithRestrictions){case (sr, pp) => sr.addRestriction(pp) match {
            case Right(value) => value
            case Left(value) =>
              logger.info(s"$pp was not added as restriction")
              value
          }}
        EfficientEjeProgresiva(pointsProgresive)
      }
    }


  protected def getSequenceElements: Either[Seq[Exception],EfficientSeqEjeElements]
  protected def getSequenceProgresivePoint: Iterable[ProgresivePoint]
}
