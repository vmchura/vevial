package relevamiento

import elementdata.TElementData

trait TSimpleRelevamiento[A <: TElementData[A]]  {
  def elements: Seq[A]
  def sliceBy(minX: Double, maxX: Double, minY: Double, maxY: Double): TSimpleRelevamiento[A]
}


