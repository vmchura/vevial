package gym
import environment.BaseEnvironmentTyped.Reward

object FantasyParabolicEnvironment extends ProblemDivisbleEnvironment[FantasyParabolicProblem,FantasyParabolicData] {
  override def numCuts: Int = 3

  override def minLengthDivisible: Double = 20

  override def maxLengthDivisible: Double = 100



  override def lengthChunks: Double = 20

  override def limitChunksFailCriteria: Int = 5

  override def isChunkBadFormed(chunkData: List[FantasyParabolicData]): Boolean = {
    val data = chunkData.map(_.value).filter(_.abs > 2f).map(x => x*x)
    val n = data.length
    val error = if(n==0) 0 else data.sum/n
    error > 10d
  }

  override def rewardByCut: Reward = -1f
}
