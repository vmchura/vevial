package environment

import scala.util.Random

class PendulumEnvironment() extends BaseEnvironment {

  var ang_velocity_range: Array[Double] = null
  var dt: Double = 0
  var viewer: Double = 0
  var gravity: Double = 0
  var mass: Double = 0
  var length: Double = 0

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
    ang_velocity_range = Array(-2*Math.PI,2*Math.PI)
    dt = 0.05
    gravity = 9.8
    mass = 1f/3f
    length = 3f/2f
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
  val beta = -Math.PI
  val betadot = 0d
  val reward = 0f
  val observation = Array(beta,betadot)
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
  val Array(last_beta,last_betadot) = last_state
  last_action = action

  val betadot = last_betadot+0.75*(actions(action)+mass*length*gravity*Math.sin(last_beta))/(mass*length*length)*dt
  val beta = last_beta + betadot*dt
  val betaNormalized = ((beta+Math.PI+2*Math.PI*100)%(2*Math.PI)) - Math.PI
  val (b,bd) = if(betadot < ang_velocity_range(0) || betadot > ang_velocity_range(1)){
    (-Math.PI,0d)
  }else{
    (betaNormalized,betadot)
  }
  if(!(betaNormalized >= -Math.PI && betaNormalized <= Math.PI)){
    println(s"beta: $beta, betaNormalized: $betaNormalized, b: $b")
  }
  assert(betaNormalized >= -Math.PI && betaNormalized <= Math.PI)

  val reward = -(Math.abs(((b+Math.PI)%(2*Math.PI))-Math.PI))
  val observation = Array(b,bd)
  val is_terminal = false
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
