package gym

import environment.BaseEnvironmentTyped.Reward

trait ProblemDivisbleEnvironment[A <: ProblemDivisible[A,D],D <: DataDivisble] {
  def numCuts: Int
  def minLengthDivisible: Double
  def maxLengthDivisible: Double

  def lengthChunks: Double
  def limitChunksFailCriteria: Int
  def isChunkBadFormed(chunkData: List[D]): Boolean = {
    val data = chunkData.map(_.value).filter(_.abs > 2f).map(x => x*x)
    val n = data.length
    val error = if(n==0) 0 else data.sum/n
    error > 10d
  }
  def rewardByCut: Reward

}
