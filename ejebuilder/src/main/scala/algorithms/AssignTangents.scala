package algorithms

import java.io.File

import io.vmchura.vevial.EjeVialBuilder.TConvertibleToEje
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.TEfficientSeqEjeElementsProgresiva

object AssignTangents {
  /**
    *
    * @param ref:     Eje Referencial
    * @param files:   Data
    * @return EjeMejorado
    */
  def calcEjeWithBasicMath(ref: TEfficientSeqEjeElementsProgresiva, files: Seq[File]): TConvertibleToEje = {
    /**
      * file => Relevamiento
      * Relevamiento => RelevamientoConProgresiva
      * RelevamientoConProgresiva => Seq elements + Tangent
      * Seq elements + Tangent => Sorted by progresiva
      * Pick a point with tangent defined
      * Pick the closest point with higher progresiva and tangent defined
      * at most D meters
      * (drop points in the middle)
      * (p,q): r: p -/- q
      *
      * if r is not correctly defined, asume r,no direction
      *
      */
    ???
  }
}
