package agent

import gym.{DataDivisble, FantasyParabolicEnvironment, ProblemDivisible}
import tiles.TileCoderTyped

class AgentBuilderTyped[P <: ProblemDivisible[P,D],D <: DataDivisble](val numActions: Int,
                                                                      val gamma: Float,
                                                                      val actorStepSizeParam: Float,
                                                                      val criticStepSizeParam: Float,
                                                                      val tileCoder: TileCoderTyped[P],
                                                                      val defaultObs: P) extends BaseAgentTyped[P]{
}
object AgentBuilderTyped {

  def buildTileCoder[P <: ProblemDivisible[P,D],D <: DataDivisble](ihtSizeParam: Int, numTilingsParam: Int, numTilesParam: Int): TileCoderTyped[P] = {
    new TileCoderTyped[P] {
      override val ihtSize: Int = ihtSizeParam

      override val numTilings: Int = numTilingsParam

      override val numTiles: Int = numTilesParam

      override def buildObs(input: P): Seq[Float] = {
        val BAD_CHUNKS_MIN = 0f
        val BAD_CHUNKS_MAX: Float = FantasyParabolicEnvironment.limitChunksFailCriteria

        val minLength = input.environment.minLengthDivisible
        val maxLength = input.environment.maxLengthDivisible
        val lengthIsTooSmall = if(input.totalLength <= minLength) 1f else 0f
        val lengthIsTooBig = if(input.totalLength >= maxLength) 1f else 0f
        val lengthToConsider: Float = (Math.min(maxLength,Math.max(minLength,input.totalLength))/(maxLength - minLength)).toFloat



        val subProblems = input.calcSubDivisions().map(_.chunksDontPassLimitCriteria()).map(_/(BAD_CHUNKS_MAX-BAD_CHUNKS_MIN))
        subProblems ::: List(lengthIsTooSmall,lengthIsTooBig,lengthToConsider)
      }
    }
  }

}