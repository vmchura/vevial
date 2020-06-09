package gym

import environment.BaseEnvironmentTyped.Reward

trait ProblemDivisbleEnvironment[A <: ProblemDivisible[A,D],D <: DataDivisble] {
  def numCuts: Int
  def minLengthDivisible: Double
  def maxLengthDivisible: Double

  def lengthChunks: Double
  def limitChunksFailCriteria: Int
  def isChunkBadFormed(chunkData: List[D]): Boolean
  def rewardByCut: Reward

}
