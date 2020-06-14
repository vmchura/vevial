package agent

import java.io

import breeze.linalg.{Axis, DenseMatrix, Vector, sum}
import environment.BaseEnvironmentTyped.Action
import softmax.SoftmaxTyped
import tiles.TileCoderTyped

trait AgentEvaluator[OBS] {
  type StateType = Vector[Double]
  def tileCoder: TileCoderTyped[OBS]
  def actorW: DenseMatrix[Double]


  def saveActorWeightOnFile(path: String): Unit = {
    breeze.linalg.csvwrite(new io.File(path),actorW)
  }

  def observation2InputTiles(observation: OBS): StateType = {
    val active_tiles = tileCoder.getTiles(observation)
    sum(actorW(::,active_tiles.toSeq),Axis._1)
  }
  def agent_policy(input: OBS): Action = {

    val state_action_preferences: StateType = observation2InputTiles(input)
    val softmax_prob = SoftmaxTyped.compute_softmax_prob(state_action_preferences)

    SoftmaxTyped.sample(softmax_prob)
  }


}
