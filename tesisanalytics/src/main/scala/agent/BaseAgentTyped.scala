package agent

import breeze.linalg.{Axis, DenseMatrix, DenseVector, Vector, sum}
import environment.BaseEnvironmentTyped._
import softmax.SoftmaxTyped
import tiles.TileCoderTyped


trait BaseAgentTyped[OBS] {
  type StateType = Vector[Double]

  def numActions: Int
  def gamma: Float
  def actorStepSizeParam: Float
  def criticStepSizeParam: Float
  def defaultObs: OBS
  def tileCoder: TileCoderTyped[OBS]

  private val actions: Iterable[Action] = 0 until numActions
  private val actorW = DenseMatrix.zeros[Double](numActions,tileCoder.ihtSize)
  private val criticW = DenseVector.zeros[Double](tileCoder.ihtSize)
  //αω: actorStepSize
  private val actorStepSize = actorStepSizeParam/tileCoder.numTilings
  //αθ: criticStepSize
  private val criticStepSize = criticStepSizeParam/tileCoder.numTilings

  private var lastObservation: OBS = defaultObs
  private var lastAction: Action = -1
  private var I: Float = 0f


  def agentStart(firstObservation: OBS): Action = {
    lastObservation = firstObservation
    agent_policy(firstObservation)
  }

  /**
    * A step taken by the agent.
    *
    * @param rewardFeatureState  given by the environment by taking lastAction
    */
  def agent_step(rewardFeatureState: RewardFeatureState[OBS]): Action = {
    val RewardFeatureState(reward,newObservation,state) = rewardFeatureState



    val prev_tiles = tileCoder.getTiles(lastObservation)


    val vPrime = if(state == TerminalState.Finished)  0f
                  else {
                    val active_tiles = tileCoder.getTiles(rewardFeatureState.observation)
                    sum(criticW(active_tiles.toSeq))
                  }
    val v = sum(criticW(prev_tiles.toSeq))
    val delta = reward + gamma*vPrime - v


    criticW(prev_tiles.toSeq) += actorStepSize*delta



    val prev_state_action_preferences = observation2InputTiles(lastObservation)
    val softmax_prob = SoftmaxTyped.compute_softmax_prob(prev_state_action_preferences)

    actions.foreach{ a =>
      if(a == lastAction){
        actorW(a,prev_tiles.toSeq) += criticStepSize*I*delta*(1-softmax_prob(a))
      }else{
        actorW(a,prev_tiles.toSeq) += criticStepSize*I*delta*(0-softmax_prob(a))
      }

    }

    val current_action = agent_policy(newObservation)


    lastObservation = newObservation
    I = gamma*I
    lastAction = current_action
    lastAction

  }

  def observation2InputTiles(observation: OBS): StateType = {
    val active_tiles = tileCoder.getTiles(observation)
    sum(actorW(::,active_tiles.toSeq),Axis._1)
  }

  def startEpisode(): Unit = {
    I = 1f
  }

  def agent_policy(input: OBS): Action = {

    val state_action_preferences: StateType = observation2InputTiles(input)
    val softmax_prob = SoftmaxTyped.compute_softmax_prob(state_action_preferences)

    SoftmaxTyped.sample(softmax_prob)
  }

}
