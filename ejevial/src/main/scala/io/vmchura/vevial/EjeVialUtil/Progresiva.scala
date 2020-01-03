package io.vmchura.vevial.EjeVialUtil

case class Progresiva(progresiva: Int) {
  def show(withSpaces: Boolean, withKmLeftPadding: Int = 0): String = {
    val km = progresiva / 1000
    val m = progresiva % 1000

    val m_str: String = (1 to (3 - m.toString.length)).map(_ => "0").mkString("") + m.toString
    val km_str: String = (1 to (withKmLeftPadding - km.toString.length)).map(_ => if (withSpaces) " " else "0").mkString("") + km.toString
    s"$km_str+$m_str"
  }
}