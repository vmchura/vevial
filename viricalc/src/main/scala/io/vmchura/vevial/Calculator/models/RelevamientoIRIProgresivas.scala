package io.vmchura.vevial.Calculator.models

import java.util.UUID

import io.vmchura.vevial.Calculator.algorithms.{AlgorithmAssignProgresivas, DataWithProgresivaSeq}
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import io.vmchura.vevial.elementdata.{DataWithPoint, IRIElementData, TElementWithPoint}
import io.vmchura.vevial.relevamiento.RelevamientoIRI

class RelevamientoIRIProgresivas[T <: TElementWithPoint[T]](val fileID: String,
                                                     val relevamientoIRI: RelevamientoIRI[T],
                                                     ejeVial: EfficientEjeProgresiva)
  extends DataWithProgresivaSeq[IRIElementDataProgresiva[T]]{


  override val elements: Seq[IRIElementDataProgresiva[T]] = {
    val dataWithProg = AlgorithmAssignProgresivas.simpleAlgorithm(relevamientoIRI.elements,relevamientoIRI.interval,ejeVial)

    dataWithProg.elements.zipWithIndex.map { case ((prog,e),indx)  =>
      IRIElementDataProgresiva(e, indx, fileID, prog)
    }
  }

  val (minProg,maxProg) = {
    val prog = elements.map(_.progresiva)
    (prog.min,prog.max)
  }

  override val isForward = elements.head.progresiva < elements.last.progresiva


}
