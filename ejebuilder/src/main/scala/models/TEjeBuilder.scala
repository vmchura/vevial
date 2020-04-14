package models

import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.TEfficientSeqEjeElementsProgresiva
import io.vmchura.vevial.elementdata.TElementData
import io.vmchura.vevial.relevamiento.TSimpleRelevamiento

trait TEjeBuilder[+A <: TSimpleRelevamiento[B],B <: TElementData[B]] {
  def relevamientos: Seq[A]
  def buildEje(): MutableEje
}
