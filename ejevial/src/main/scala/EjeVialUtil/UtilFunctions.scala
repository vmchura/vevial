package EjeVialUtil

object UtilFunctions {
  def convertIntToProgresivaString(prog: Int, padding: String = ""): String = {
    val metros = prog%1000
    val kmStr = (prog/1000).toString
    val metrosStr = metros match {
      case m if m == 0 => "000"
      case m => f"$m%03d"
    }
    s"$kmStr+$metrosStr"

  }
  def str2Double(str: String): Option[Double] = try { Some(str.toDouble) } catch{ case _ : Throwable => None }

}
