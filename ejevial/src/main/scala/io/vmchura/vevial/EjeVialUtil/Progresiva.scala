package io.vmchura.vevial.EjeVialUtil

case class Progresiva(progresiva: Int) {
  def show(withSpaces: Boolean, withKmLeftPadding: Int = 0): String = {
    val sign = progresiva.sign
    val progresivaToUse = progresiva.abs
    val km = progresivaToUse / 1000
    val m = progresivaToUse % 1000

    val m_str: String = (1 to (3 - m.toString.length)).map(_ => "0").mkString("") + m.toString
    val km_str: String = (1 to (withKmLeftPadding - km.toString.length)).map(_ => if (withSpaces) " " else "0").mkString("") + km.toString
    s"${if (sign < 0) "-" else ""}$km_str+$m_str"
  }
}

object Progresiva {
  def apply(progresiva: Int): Progresiva = new Progresiva(progresiva)
  def apply(progresiva: String): Option[Progresiva] = {
    Option.when(progresiva.contains('+') && progresiva.replace('+','.').toDoubleOption.isDefined) {
      val progresivaInt = progresiva.split('+').map(_.trim).reverse.zipWithIndex.map {
        case (chunk, index) =>
          Math.pow(1e3, index) * chunk.toInt
      }.sum.toInt
      new Progresiva(progresivaInt)
    }
  }
}