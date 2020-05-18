package io.vmchura.vevial.models

import io.vmchura.vevial.EjeVialUtil.Progresiva
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.TEfficientSeqEjeElementsProgresiva
import io.vmchura.vevial.algorithms.{AlgorithmAssignProgresivas, DataWithProgresivaSeq}
import io.vmchura.vevial.elementdata.TElementWithPoint
import io.vmchura.vevial.relevamiento.RelevamientoIRI

class RelevamientoIRIProgresivas[T <: TElementWithPoint[T]](val fileID: String,
                                                     val relevamientoIRI: RelevamientoIRI[T],
                                                     ejeVial: TEfficientSeqEjeElementsProgresiva, progFrom: Progresiva, progTo: Progresiva)
  extends DataWithProgresivaSeq[IRIElementDataProgresiva[T]]{


  override val elements: Seq[IRIElementDataProgresiva[T]] = {
    val dataWithProg = AlgorithmAssignProgresivas.simpleAlgorithm(relevamientoIRI.elements,relevamientoIRI.interval,ejeVial)

    val dataFiltered = dataWithProg.elements.filter{case (p,_) => progFrom.progresiva <= p && p <= progTo.progresiva}
    if(dataFiltered.isEmpty)
      throw new IllegalArgumentException("Los datos relevados no coinciden con el tramo seleccionado")

    dataFiltered.zipWithIndex.map { case ((prog,e),indx)  =>
      IRIElementDataProgresiva(e, indx, fileID, prog)
    }
  }

  val (minProg,maxProg) = {
    val prog = elements.map(_.progresiva)
    (prog.min,prog.max)
  }

  override val isForward: Boolean = elements.head.progresiva < elements.last.progresiva


}
