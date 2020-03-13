package agent

/**
  * Implements the agent for an RL-Glue environment.
  * Note:
  * agent_init, agent_start, agent_step, agent_end, agent_cleanup, and
  * agent_message are required methods.
  *
  */
trait BaseAgent extends Serializable{

  /**
    * Setup for the agent called when the experiment first starts
    * @param agentInfo
    */
  def agent_init(agentInfo: AgentInfo): Unit

  /**
    * The first method called when the experiment starts, called after
    * the environment starts.
    *
    * @param observation  (Numpy array): the state observation from the environment's evn_start function.
    * @return             The first action the agent takes.
    */
  def agent_start(observation: Array[Double]): Int

  /**
    * A step taken by the agent.
    *
    * @param reward      the reward received for taking the last action taken
    * @param observation (Numpy array): the state observation from the
    *                    environment's step based, where the agent ended up after the
    *                    last step
    * @return             The action the agent is taking.
    */
  def agent_step(reward: Float, observation: Array[Double]): Int

  /**
    * Run when the agent terminates.
    * @param reward       the reward the agent received for entering the terminal state.
    */
  def agent_end(reward: Float): Unit

  /**
    * Cleanup done after the agent ends.
    */
  def agent_cleanup(): Unit

  /**
    * A function used to pass information from the agent to the experiment.
    * @param message    The message passed to the agent.
    * @return           The response (or answer) to the message.
    */
  def agent_message(message: String): Double

}
