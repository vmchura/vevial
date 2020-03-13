package RFunctionDefiner

import org.ddahl.rscala.RClient

object RInitializer {
  implicit lazy val R: RClient =  org.ddahl.rscala.RClient()
  private val RDefinitions: Seq[RWrapper] = List(PlotHistogram,PlotRLEvolution)

  private var initAlready: Boolean = false

  /**
    * It will only be executed the first time, next calls it will pass
    */
  def init(): Unit ={
    if(!initAlready) {
      RDefinitions.foreach(_.init)
    }
    initAlready = true
  }


}
