package gym

import environment.BaseEnvironmentTyped.{EnvironmentError, Reward}

trait ProblemDivisible[A <: ProblemDivisible[A,D], D <: DataDivisble] {this: A =>
  def environment: ProblemDivisbleEnvironment[A,D]
  def data: List[D]
  def minData: Double
  def maxData: Double
  def subDivisionsCutAt: Array[Double]
  def calcSubDivisions(): List[A]
  def cutAt(nCut: Int): Either[EnvironmentError,(Reward,Option[A])]

  val totalLength: Double = maxData - minData
  def calcChunks(): List[List[D]]
  def chunksDontPassLimitCriteria(): Int
  def rewardByCurrentDistribution: Reward
}
