package algorithms

import gym.DataDivisble

class DataWithProjectionError (progresiva: Double, errorProjection: Float) extends DataDivisble {
  override def value: Float = errorProjection

  override def toCompare: Double = progresiva

  override def toString: String = f"Projection[$progresiva%.2f, $errorProjection%.2f]"
}

object DataWithProjectionError {
  def buildFromBasicSectionBuilder(basicSectionBuilder: BasicSectionBuilder): Either[Exception,AxisFitProblem] = {
    val res = for{
      min <- basicSectionBuilder.dataToMatch.map(_.prog).minOption
      max <- basicSectionBuilder.dataToMatch.map(_.prog).maxOption
    }yield{
      AxisFitProblem(basicSectionBuilder.in,basicSectionBuilder.out,basicSectionBuilder.dataToMatch,min,max)
    }

    res.getOrElse(Left(new IllegalArgumentException("BasicSectionBuilder data is empty")))

  }
}