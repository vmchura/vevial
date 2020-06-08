package environment

import environment.BaseEnvironmentTyped.{RewardFeatureState, StateIsUnknow}
import org.scalatest.flatspec.AnyFlatSpec
import breeze.linalg.Vector
class LinearEnvironmentTypedTest extends AnyFlatSpec {
  import BaseEnvironmentTyped.TerminalState._
  behavior of "LinearEnvironment Typed"
  it should "Not be initilized" in {
    val le = new LinearEnvironmentTyped()
    assert(le.env_step(1) match {
      case Left(StateIsUnknow(_)) => true
      case _ => false
    })

  }

  it should "Be 0 the first observation" in {
    val le = new LinearEnvironmentTyped()
    val obs = le.env_start()

    assertResult(1)(obs)
  }
  it should "Advance 1 position" in {
    val le = new LinearEnvironmentTyped()
    val _ = le.env_start()
    assert(le.env_step(2) match {
      case Right(RewardFeatureState(1f, observation, OnProcess)) =>
        assertResult(Vector(1d))(observation)
        true
      case _ =>
        false
    })
  }
  it should "Advance 9 position and reach to final" in {
    val le = new LinearEnvironmentTyped()
    val _ = le.env_start()
    (1 to 9).foreach(_ => le.env_step(2))
    assert(le.env_step(2) match {
      case Right(RewardFeatureState(1f, observation, Finished)) =>
        assertResult(Vector(10d))(observation)
        true
      case _ =>
        false
    })
  }
}
