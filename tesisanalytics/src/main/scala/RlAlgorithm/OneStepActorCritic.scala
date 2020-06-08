package RlAlgorithm

import agent.LinearAgentTyped
import com.typesafe.scalalogging.Logger
import environment.BaseEnvironmentTyped._
import environment.LinearEnvironmentTyped


object OneStepActorCritic {
  import TerminalState._

  private val logger = Logger(this.getClass)
  def runExperimentLinearStrip(ihtSize: Int, numTilings: Int, numTiles: Int,
                    gamma: Float,actorStepSize: Float,criticStepSize: Float,
                               maxNumEpisodes: Int): Iterable[Float] = {

    val tileCoder = LinearAgentTyped.buildTileCoder(ihtSize,numTilings,numTiles)
    val agent = new LinearAgentTyped(3,gamma,actorStepSize,criticStepSize,tileCoder)
    val env = new LinearEnvironmentTyped()

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
}
