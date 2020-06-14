package gym

import environment.BaseEnvironmentTyped.{EnvironmentError, Reward}

trait ProblemDivisible[A <: ProblemDivisible[A,D], D <: DataDivisble] {this: A =>
  def environment: ProblemDivisbleEnvironment[A,D]
  def data: List[D]
  def minData: Double
  def maxData: Double
  def subDivisionsCutAt: Array[Double] = {
    val delta = (maxData - minData)/(environment.numCuts+1)
    Array.range(1,environment.numCuts+1).map{ i =>  i*delta + minData}
  }
  def calcSubDivisions(): List[A]
  def cutAt(nCut: Int): Either[EnvironmentError,(Reward,Option[A])]

  val totalLength: Double = maxData - minData
  def calcChunks(): List[List[D]] = {
    val L = environment.lengthChunks
    val delta = maxData - minData
    val k = (delta/L).toInt + 1

    (0 until k).map(i => {
      val ini = i*L + minData
      val end = if(i == k-1) maxData else ini + L
      data.filter(d => ini <= d.toCompare && d.toCompare < end)
    }).toList

  }
  def chunksDontPassLimitCriteria(): Int  = Math.min(calcChunks().count(environment.isChunkBadFormed),environment.limitChunksFailCriteria)
  def rewardByCurrentDistribution: Reward
}
