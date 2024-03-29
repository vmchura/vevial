package io.vmchura.vevial.algorithms

import io.vmchura.vevial.elementdata.{DataWithPoint, TCrudeIRIData}

case class CrudeDataWithProgresiva(crudeData: TCrudeIRIData, prog: Int)

trait DataWithProgresivaSeq[T]{
  def elements: Seq[T]
  def isForward: Boolean
}


