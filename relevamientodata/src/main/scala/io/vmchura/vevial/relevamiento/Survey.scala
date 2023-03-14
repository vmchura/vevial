package io.vmchura.vevial.relevamiento

import io.vmchura.vevial.elementdata.{GPXElementData, TElementData}

trait Survey[T <: TElementData[T]]{
  def surveyInformation: List[T]
}
