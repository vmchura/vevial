package io.vmchura.vevial.Calculator.models


import io.vmchura.vevial.elementdata.IRIElementData

/**
  *
  * @param certainty: 0: Perfect, <4 Very Good,  <10 Good
  */
case class IRIElementDataOptProgresiva[T](iriElementData: T, indx: Int, progresiva: Option[Int], certainty: Double)
case class IRIElementDataProgresiva[T](iriElementData: T, indx: Int, fileTag: String,progresiva: Int)
