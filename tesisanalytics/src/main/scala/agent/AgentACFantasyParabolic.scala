package agent

import gym.{FantasyParabolicEnvironment, FantasyParabolicProblem}
import tiles.TileCoderTyped

class AgentACFantasyParabolic(val numActions: Int,
                              val gamma: Float,
                              val actorStepSizeParam: Float,
                              val criticStepSizeParam: Float,
                              val tileCoder: TileCoderTyped[FantasyParabolicProblem]) extends BaseAgentTyped[FantasyParabolicProblem]{
  override def defaultObs: FantasyParabolicProblem = new FantasyParabolicProblem(Nil)
}
object AgentACFantasyParabolic {

  def buildTileCoder(ihtSizeParam: Int, numTilingsParam: Int, numTilesParam: Int): TileCoderTyped[FantasyParabolicProblem] = {
    new TileCoderTyped[FantasyParabolicProblem] {
      override val ihtSize: Int = ihtSizeParam

      override val numTilings: Int = numTilingsParam

      override val numTiles: Int = numTilesParam

      override def buildObs(input: FantasyParabolicProblem): Seq[Float] = {
        val BAD_CHUNKS_MIN = 0f
        val BAD_CHUNKS_MAX: Float = FantasyParabolicEnvironment.limitChunksFailCriteria
        val subProblems = input.calcSubDivisions().map(_.chunksDontPassLimitCriteria()).map(_/(BAD_CHUNKS_MAX-BAD_CHUNKS_MIN))

        subProblems
      }
    }
  }

}