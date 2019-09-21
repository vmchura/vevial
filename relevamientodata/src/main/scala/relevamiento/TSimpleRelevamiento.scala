package relevamiento

import elementdata.TElementData

trait TSimpleRelevamiento[A <: TElementData[A]]  {
  def elements: Seq[A]
}


