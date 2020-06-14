package algorithms

import environment.BaseEnvironmentTyped.Reward
import gym.ProblemDivisbleEnvironment

object AxisFitEnvironment extends ProblemDivisbleEnvironment[AxisFitProblem,DataWithProjectionError]{
  override def numCuts: Int = 3

  override def minLengthDivisible: Double = 60

  override def maxLengthDivisible: Double = 100

  override def lengthChunks: Double = 20

  override def limitChunksFailCriteria: Int = 5

  override def rewardByCut: Reward = -0.5f
}
