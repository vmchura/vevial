package agent

import softmax.SoftMax
import tiles.PendulumTileCoder

import scala.util.Random

class ActorCriticSoftmaxAgent() extends BaseAgent {

  var num_tilings: Int = -1// agentInfo.num_tilings
  var tc: PendulumTileCoder =  null//new PendulumTileCoder(iht_size = agentInfo.iht_size,num_tilings = num_tilings,num_tiles = agentInfo.num_tiles)
  var actor_step_size: Float = -1f// = agentInfo.actor_step_size/num_tilings
  var critic_step_size: Float = -1f //agentInfo.critic_step_size/num_tilings
  var avg_reward_step_size: Float = -1f// = agentInfo.avg_reward_step_size
  var actions: List[Int] = null//agentInfo
  var iht_size: Int = -1
  var num_tiles: Int = -1


  var avg_reward: Double = 0d
  var actor_w: Array[Array[Double]] = null
  var critic_w: Array[Double] = null

  var softmax_prob: Array[Double] = null
  var prev_tiles: Array[Int] = null
  var last_action: Int = -1

  override def toString: String = {
    s"""
      | agent next_action: ${last_action}
      | agent avg_reward: ${avg_reward}
      | agent first 10 values of actor weights[0] [${actor_w(0).take(10).mkString(" ")}]
      | agent first 10 values of actor weights[1] [${actor_w(1).take(10).mkString(" ")}]
      | agent first 10 values of actor weights[2] [${actor_w(2).take(10).mkString(" ")}]
      | agent first 10 values of critic weights [${critic_w.take(10).mkString(" ")}]
      |""".stripMargin
  }
  /**
    * Setup for the agent called when the experiment first starts
    *
    * @param agentInfo
    */
  override def agent_init(agentInfo: AgentInfo): Unit = {
    // set random seed for each run
    Random.setSeed(agentInfo.seed)
    num_tilings =  agentInfo.num_tilings
    iht_size = agentInfo.iht_size
    num_tiles = agentInfo.num_tiles
    //initialize self.tc to the tile coder we created
    tc =  new PendulumTileCoder(iht_size = agentInfo.iht_size,num_tilings = num_tilings,num_tiles = agentInfo.num_tiles)

    //set step-size accordingly
    // (we normally divide actor and critic step-size by num. tilings (p.217-218 of textbook))
    actor_step_size = agentInfo.actor_step_size/num_tilings
    critic_step_size = agentInfo.critic_step_size/num_tilings
    avg_reward_step_size  = agentInfo.avg_reward_step_size
    actions = (0 until agentInfo.num_actions).toList


    /**
      * # Set initial values of average reward, actor weights, and critic weights
      * # We initialize actor weights to three times the iht_size.
      * # Recall this is because we need to have one set of weights for each of the three actions.
      */

    avg_reward = 0d
    actor_w = Array.fill(actions.length)(Array.fill(iht_size)(0d))
    critic_w = Array.fill(iht_size)(0d)

    softmax_prob = null
    prev_tiles = null
    last_action = -1
  }

  /**
    * policy of the agent
    * @param active_tiles (Numpy array): active tiles returned by tile coder
    * @return             The action selected according to the policy
    */
  def agent_policy(active_tiles: Array[Int]): Int = {

    //compute softmax probability
    softmax_prob = SoftMax.compute_softmax_prob(actor_w,active_tiles)

    /**
      * # Sample action from the softmax probability array
      * # self.rand_generator.choice() selects an element from the array with the specified probability
      */

    SoftMax.sample(softmax_prob)
  }

  /**
    * The first method called when the experiment starts, called after
    * the environment starts.
    *
    * @param observation (Numpy array): the state observation from the environment's evn_start function.
    * @return The first action the agent takes.
    */
  override def agent_start(observation: Array[Double]): Int = {
    val Array(angle,ang_vel) = observation

    /**
      * ### Use self.tc to get active_tiles using angle and ang_vel (2 lines)
      * # set current_action by calling self.agent_policy with active_tiles
      * # active_tiles = ?
      * # current_action = ?
      */
    val active_tiles = tc.get_tiles(angle.toFloat,ang_vel.toFloat)
    val current_action = agent_policy(active_tiles)

    last_action = current_action
    prev_tiles = active_tiles.clone()

    last_action

  }

  /**
    * A step taken by the agent.
    *
    * @param reward      the reward received for taking the last action taken
    * @param observation (Numpy array): the state observation from the
    *                    environment's step based, where the agent ended up after the
    *                    last step
    * @return The action the agent is taking.
    */
  override def agent_step(reward: Float, observation: Array[Double]): Int = {
    val Array(angle,ang_vel) = observation

    /**
      *  Use self.tc to get active_tiles using angle and ang_vel (1 line)
      */
    val active_tiles = tc.get_tiles(angle.toFloat,ang_vel.toFloat)

    /**
      * Compute delta using Equation (1) (1 line)
      * delta = reward - self.avg_reward +
      *         self.critic_w[active_tiles].sum() - self.critic_w[self.prev_tiles].sum()
      */

    val delta = reward - avg_reward +
                active_tiles.map(critic_w).sum - prev_tiles.map(critic_w).sum

    /**
      * update average reward using Equation (2) (1 line)
      * # self.avg_reward += ?
      * ### START CODE HERE ###
      *         self.avg_reward += self.avg_reward_step_size * delta
      */

    avg_reward += avg_reward_step_size*delta


    /**
      * update critic weights using Equation (3) and (5) (1 line)
      * # self.critic_w[self.prev_tiles] += ?
      * ### START CODE HERE ###
      *         self.critic_w[self.prev_tiles] += self.critic_step_size * delta
      */

    prev_tiles.foreach{ p =>
      critic_w(p) += critic_step_size*delta
    }

    /**
      * update actor weights using Equation (4) and (6)
      * # We use self.softmax_prob saved from the previous timestep
      * # We leave it as an exercise to verify that the code below corresponds to the equation.
      *
      * for a in self.actions:
      * if a == self.last_action:
      *                 self.actor_w[a][self.prev_tiles] +=
      *                   self.actor_step_size * delta * (1 - self.softmax_prob[a])
      * else:
      *                 self.actor_w[a][self.prev_tiles] +=
      *                   self.actor_step_size * delta * (0 - self.softmax_prob[a])
      */

    actions.foreach{ a =>
      prev_tiles.foreach{ p =>
        if(a == last_action){
          actor_w(a)(p) += actor_step_size*delta*(1-softmax_prob(a))
        }else{
          actor_w(a)(p) += actor_step_size*delta*(0-softmax_prob(a))
        }
      }

    }

    /**
      * set current_action by calling self.agent_policy with active_tiles (1 line)
      * # current_action = ?
      * ### START CODE HERE ###
      * current_action = self.agent_policy(active_tiles)
      */

    val current_action = agent_policy(active_tiles)

    prev_tiles = active_tiles
    last_action = current_action
    last_action




  }

  /**
    * Run when the agent terminates.
    *
    * @param reward the reward the agent received for entering the terminal state.
    */
  override def agent_end(reward: Float): Unit = ()

  /**
    * Cleanup done after the agent ends.
    */
  override def agent_cleanup(): Unit = ()

  /**
    * A function used to pass information from the agent to the experiment.
    *
    * @param message The message passed to the agent.
    * @return The response (or answer) to the message.
    */
  override def agent_message(message: String): Double = {
    if(message.equals("get avg reward")){
      avg_reward
    }else{
      -1d
    }
  }
}
