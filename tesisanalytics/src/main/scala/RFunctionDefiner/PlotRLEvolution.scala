package RFunctionDefiner

object PlotRLEvolution extends RWrapper{

  def drawEvolutionRL(dataX: Array[Double],dataY: Array[Double],
                           mainTitle: String,
                           xLabel: String, yLabel: String,
                           result: String)(implicit  R : org.ddahl.rscala.RClient): Unit = {
    require(result.endsWith(".png"))
    R.eval("drawEvolutionRL(%-,%-,%-,%-,%-,%-)",
      dataX,dataY,mainTitle,xLabel,yLabel,result)

  }

  override def init(implicit R: org.ddahl.rscala.RClient): Unit = {
    R.eval("source(\"/home/vmchura/Documents/001.Projects/vevial/tesisanalytics/src/main/scala/softmax/DrawEvolutionRL.R\")")
  }
}
