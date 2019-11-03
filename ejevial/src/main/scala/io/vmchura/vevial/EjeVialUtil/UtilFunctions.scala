package io.vmchura.vevial.EjeVialUtil

import com.typesafe.scalalogging.Logger

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
  def time[R](logger: Logger)(prefix: String, block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    logger.info(f"$prefix%s Elapsed time:  ${((t1 - t0)/1000000.0)}%.2f ms")
    result
  }

}
