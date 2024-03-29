package gym
import environment.BaseEnvironmentTyped.Reward

object FantasyParabolicEnvironment extends ProblemDivisbleEnvironment[FantasyParabolicProblem,FantasyParabolicData] {
  override def numCuts: Int = 3

  override def minLengthDivisible: Double = 20

  override def maxLengthDivisible: Double = 100

  override def lengthChunks: Double = 20

  override def limitChunksFailCriteria: Int = 5

  override def rewardByCut: Reward = -0.5f
}
