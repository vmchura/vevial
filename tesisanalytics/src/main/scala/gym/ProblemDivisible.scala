package gym

import environment.BaseEnvironmentTyped.{EnvironmentError, Reward}

trait ProblemDivisible[A <: ProblemDivisible[A,D], D <: DataDivisble] {this: A =>
  def environment: ProblemDivisbleEnvironment[A,D]
  def data: List[D]
  def subDivisions: Array[Double]
  def cutAt(nCut: Int): Either[EnvironmentError,(Reward,Option[A])]
  val minData: D = data.minBy(_.toCompare)
  val maxData: D = data.maxBy(_.toCompare)
  val totalLength: Double = maxData.toCompare - minData.toCompare
  def calcChunks(): List[List[D]]
  def rewardByCurrentDistribution: Reward
}
