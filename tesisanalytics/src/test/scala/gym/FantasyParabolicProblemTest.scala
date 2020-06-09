package gym

import RlAlgorithm.OneStepActorCritic
import environment.BaseEnvironmentTyped.{RewardFeatureState, StateIsUnknow}
import org.scalatest.flatspec.AnyFlatSpec
import environment.BaseEnvironmentTyped.TerminalState._
class FantasyParabolicProblemTest extends AnyFlatSpec {
  private val experimentBuilder = OneStepActorCritic.FANTASY_PARABOLIC_COMPONENT_BUILDER
  behavior of "FantasyParabolicProblem Typed"
  it should "not be initilized" in {
    val env = experimentBuilder.getEnvironment
    assert(env.env_step(1) match {
      case Left(StateIsUnknow(_)) => true
      case _ => false
    })
  }

  it should "be initilized" in {
    val env = experimentBuilder.getEnvironment
    val iniObs = env.env_start()
    assert(env.env_step(1) match {
      case Right(_) => true
      case _ => false
    })
  }
  it should "give finished/onProcess when action is made" in {
    val env = experimentBuilder.getEnvironment
    val actionResponse = Map(0 -> Finished, 1 -> OnProcess, 2 -> Finished, 3 -> OnProcess)
    actionResponse.foreach{case (a,b) =>
      val iniObs = env.env_start()
      assert(env.env_step(a) match {
        case Right(RewardFeatureState(reward, observation, x)) if x == b =>
          println(s"action $a gives $reward as reward and range: ${observation.minData} -> ${observation.maxData}")
          true
        case x =>
          println(x)
          false
      })
    }

  }
}
