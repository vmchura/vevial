package environment

import environment.BaseEnvironmentTyped.{Action, EnvironmentError, Observation, RewardFeatureState, TerminalState}

trait BaseEnvironmentTyped {

  /**
    * The first method called when the experiment starts, called before the
    * agent starts.
    *
    * @return The first state observation from the environment.
    */
  def env_start(): Observation

  /**
    * A step taken by the environment.
    *
    * @param action   The action taken by the agent
    * @return (float, state, Boolean): a tuple of the reward, state observation,
    *         and boolean indicating if it's terminal.
    */
  def env_step(action: Action): Either[EnvironmentError,RewardFeatureState]


}
object BaseEnvironmentTyped {
  type Reward = Float
  type Observation = breeze.linalg.Vector[Double]
  type Action = Int
  object TerminalState extends Enumeration {
    val Finished: TerminalState.Value = Value
    val OnProcess: TerminalState.Value = Value
    val UnknowState: TerminalState.Value = Value
  }
  case class RewardFeatureState(reward: Reward,observation: Observation,terminalState: TerminalState.Value)
  sealed trait EnvironmentError
  case class InvalidAction(msg: String) extends EnvironmentError
  object StateIsTerminal extends EnvironmentError
  case class StateIsUnknow(msg: String) extends EnvironmentError

}