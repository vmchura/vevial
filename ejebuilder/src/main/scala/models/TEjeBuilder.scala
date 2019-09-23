package models

import PlanarGeometric.ProgresiveEje.TEfficientSeqEjeElementsProgresiva
import elementdata.TElementData
import relevamiento.TSimpleRelevamiento

trait TEjeBuilder[+A <: TSimpleRelevamiento[B],B <: TElementData[B]] {
  def relevamientos: Seq[A]
  def buildEje(): TEfficientSeqEjeElementsProgresiva
}
