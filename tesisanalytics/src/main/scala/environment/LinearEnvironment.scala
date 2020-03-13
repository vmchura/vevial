package environment

import scala.util.Random

class LinearEnvironment() extends BaseEnvironment {



  var valid_actions:  Array[Int] = null
  var actions: Array[Int] = null
  var last_action = -1



  override var reward_obs_term: (Float,Array[Double], Boolean) = null

  /**
    * Setup for the environment called when the experiment first starts
    *
    * @param env_info
    */
  override def env_init(env_info: EnvInfo): Unit = {
    Random.setSeed(env_info.seed)
    valid_actions = Array(0,1,2)
    actions = Array(-1,0,1)
    last_action = -1

  }

  /**
    * The first method called when the experiment starts, called before the
    * agent starts.
    *
    * @return The first state observation from the environment.
    */
override def env_start(): Array[Double] = {

  val reward = 0f
  val observation = Array(0d)
  val is_terminal = false

  reward_obs_term = (reward,observation,is_terminal)
  reward_obs_term._2
}

  /**
    * A step taken by the environment.
    *
    * @param action The action taken by the agent
    * @return (float, state, Boolean): a tuple of the reward, state observation,
    *         and boolean indicating if it's terminal.
    */
override def env_step(action: Int): (Float,Array[Double], Boolean) = {
  assert(valid_actions.contains(action))

  val last_state = reward_obs_term._2
  val Array(last_pos) = last_state
  last_action = action


  val newPosition = (last_pos,actions(action)) match{
    case (0,-1) => 0
    case (10,1) => 10
    case (lp,ac) => lp+ac
  }


  val reward = -(10f-newPosition)
  val observation = Array(newPosition)
  val is_terminal = newPosition >= 10
  reward_obs_term = (reward.toFloat,observation,is_terminal)
  reward_obs_term

}

  /**
    * Cleanup done after the environment ends
    */
override def env_cleanup(): Unit = ()

  /**
    * A message asking the environment for information
    *
    * @param message
    * @return
    */
override def env_message(message: String): Double = -1d
}
