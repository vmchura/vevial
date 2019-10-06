package RFunctionDefiner


object PlotHistogram extends RWrapper{
  def drawAndSaveHistogram(data: Array[Double],mainTitle: String, xLabel: String, minX: Double, maxX: Double,binWidth: Double,result: String)(implicit  R : org.ddahl.rscala.RClient): Unit = {
    require(result.endsWith(".png"))
    R.eval("drawAndSaveHist(%-,%-,%-,%-,%-,%-,%-)",
      data,mainTitle,xLabel,minX,maxX,binWidth,result)

  }
  def drawAndSaveHistogram2D(dataX: Array[Double],dataY: Array[Double],
                           mainTitle: String,
                           xLabel: String, yLabel: String,
                           result: String)(implicit  R : org.ddahl.rscala.RClient): Unit = {
    require(result.endsWith(".png"))
    R.eval("drawAndSaveHist2D(%-,%-,%-,%-,%-,%-)",
      dataX,dataY,mainTitle,xLabel,yLabel,result)

  }

  override def init(implicit R: org.ddahl.rscala.RClient): Unit = {
    R.eval("source(\"/home/vmchura/Documents/001.Projects/Vevial/tesisanalytics/src/main/scala/analizetramos/DefinePlotHistogram2D.R\")")
    R.eval("source(\"/home/vmchura/Documents/001.Projects/Vevial/tesisanalytics/src/main/scala/analizetramos/DefineRPlotHisogram.R\")")
  }
}
