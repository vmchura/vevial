package environment

trait BaseEnvironment {

  var reward_obs_term: (Float,Array[Double], Boolean)

  /**
    * Setup for the environment called when the experiment first starts
    * @param env_info
    */
  def env_init(env_info: EnvInfo): Unit

  /**
    * The first method called when the experiment starts, called before the
    * agent starts.
    *
    * @return The first state observation from the environment.
    */
  def env_start(): Array[Double]

  /**
    * A step taken by the environment.
    *
    * @param action   The action taken by the agent
    * @return (float, state, Boolean): a tuple of the reward, state observation,
    *         and boolean indicating if it's terminal.
    */
  def env_step(action: Int): (Float,Array[Double], Boolean)

  /**
    * Cleanup done after the environment ends
    */
  def env_cleanup(): Unit

  /**
    * A message asking the environment for information
    * @param message
    * @return
    */
  def env_message(message: String): Double

}
