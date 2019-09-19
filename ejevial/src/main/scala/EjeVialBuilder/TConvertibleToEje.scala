package EjeVialBuilder

import PlanarGeometric.BasicEje.{EfficientSeqEjeElements, TEfficientSeqEjeElements}
import PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import PlanarGeometric.RestrictiveEje.{EjeEfficientWithRestrictions, ProgresivePoint}
import com.typesafe.scalalogging.Logger

trait TConvertibleToEje {
  final def toEje(): EfficientEjeProgresiva = {
    val sequenceWithRestrictions = EjeEfficientWithRestrictions(getSequenceElements())
    val logger = Logger(classOf[TConvertibleToEje])

    val pointsProgresive =
      getSequenceProgresivePoint().foldLeft(sequenceWithRestrictions){case (sr, pp) => sr.addRestriction(pp) match {
        case Right(value) => value
        case Left(value) => {
          logger.info(s"$pp was not added as restriction")
          value
        }
      }}

    EfficientEjeProgresiva(pointsProgresive)
  }
  protected def getSequenceElements(): EfficientSeqEjeElements
  protected def getSequenceProgresivePoint(): Iterable[ProgresivePoint]
}
