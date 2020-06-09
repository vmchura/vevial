package RlAlgorithm

import agent.BaseAgentTyped
import environment.BaseEnvironmentTyped
import tiles.TileCoderTyped

trait ExperimentBuilderComponents[A] {

  def getTileCoder(ihtSize: Int,numTilings: Int,numTiles: Int):TileCoderTyped[A]
  def getAgent(gamma: Float, actorStepSize: Float, criticStepSize: Float,tileCoder: TileCoderTyped[A]): BaseAgentTyped[A]
  def getEnvironment: BaseEnvironmentTyped[A]

}
