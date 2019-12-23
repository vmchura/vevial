package io.vmchura.vevial.Calculator.models

class SortedElementIriResult(data: Seq[ElementIriResult]) {
  val elements = data.sortBy(_.progIni)
  val minProg: Option[Int] = elements.headOption.map(_.progIni)
  val maxProg: Option[Int] = elements.lastOption.map(_.progFin)


}
