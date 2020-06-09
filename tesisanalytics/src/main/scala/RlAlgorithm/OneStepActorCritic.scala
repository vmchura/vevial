package RlAlgorithm

import agent.{AgentACFantasyParabolic, BaseAgentTyped, LinearAgentTyped}
import com.typesafe.scalalogging.Logger
import environment.BaseEnvironmentTyped._
import environment.{BaseEnvironmentTyped, EnvironmentACFantasyParabolic, LinearEnvironmentTyped}
import gym.FantasyParabolicProblem
import tiles.TileCoderTyped


object OneStepActorCritic {
  import TerminalState._

  private val logger = Logger(this.getClass)
  def runExperimentActorCriticSoftMax[A](ihtSize: Int, numTilings: Int, numTiles: Int,
                                         gamma: Float, actorStepSize: Float, criticStepSize: Float,
                                         maxNumEpisodes: Int, builderComponents: ExperimentBuilderComponents[A]): Iterable[Float] = {

    val tileCoder = builderComponents.getTileCoder(ihtSize,numTilings,numTiles)
    val agent = builderComponents.getAgent(gamma,actorStepSize,criticStepSize,tileCoder)
    val env = builderComponents.getEnvironment

    def runStep(a: Action, prevAcum: Double): Either[EnvironmentError,Double] = {

      env.env_step(a).flatMap(k => {
        val RewardFeatureState(reward,_,state) = k
        state match {
          case Finished => Right(prevAcum + reward)
          case OnProcess =>
            val newAction = agent.agent_step(k)
            runStep(newAction,reward + prevAcum)
          case other => Left(StateIsUnknow(s"the next state is invalid: $other"))
        }
      })
    }

    val data = (1 to maxNumEpisodes).map{ _ =>
      agent.startEpisode()
      val firstObservation = env.env_start()
      val firstAction = agent.agentStart(firstObservation)
      runStep(firstAction,0d)
    }

    val errores = for(Left(i) <- data) yield i
    val rewardsPerEpisode = for(Right(i) <- data) yield i


    logger.debug("Errors on runExperimentLinearStrip")
    logger.debug(errores.mkString("\n"))
    logger.debug("End of errors")
    rewardsPerEpisode.map(_.toFloat)


  }

  val LINEAR_STRIP_COMPONENT_BUILDER: ExperimentBuilderComponents[Int] = new ExperimentBuilderComponents[Int] {
    override def getTileCoder(ihtSize: Int, numTilings: Int, numTiles: Int): TileCoderTyped[Int] = LinearAgentTyped.buildTileCoder(ihtSize,numTilings,numTiles)

    override def getAgent(gamma: Float, actorStepSize: Float, criticStepSize: Float, tileCoder: TileCoderTyped[Int]): BaseAgentTyped[Int] = new LinearAgentTyped(3,gamma,actorStepSize,criticStepSize,tileCoder)

    override def getEnvironment: BaseEnvironmentTyped[Int] = new LinearEnvironmentTyped()
  }

  val FANTASY_PARABOLIC_COMPONENT_BUILDER: ExperimentBuilderComponents[FantasyParabolicProblem] = new ExperimentBuilderComponents[FantasyParabolicProblem] {
    override def getTileCoder(ihtSize: Int, numTilings: Int, numTiles: Int): TileCoderTyped[FantasyParabolicProblem] = AgentACFantasyParabolic.buildTileCoder(ihtSize,numTilings,numTiles)

    override def getAgent(gamma: Float, actorStepSize: Float, criticStepSize: Float, tileCoder: TileCoderTyped[FantasyParabolicProblem]): BaseAgentTyped[FantasyParabolicProblem] = new AgentACFantasyParabolic(4,gamma,actorStepSize,criticStepSize,tileCoder)

    override def getEnvironment: BaseEnvironmentTyped[FantasyParabolicProblem] = new EnvironmentACFantasyParabolic()
  }

}

