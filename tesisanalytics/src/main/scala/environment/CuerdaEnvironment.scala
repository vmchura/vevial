package environment

import scala.util.Random

class CuerdaEnvironment() extends BaseEnvironment {

  val nodesToTry = 10
  var range_points: (Int,Int) = null


  var valid_actions:  Array[Int] = null
  var actions: Array[(Int,Int)] = null
  var last_action = -1



  override var reward_obs_term: (Float,Array[Double], Boolean) = null

  /**
    * Setup for the environment called when the experiment first starts
    *
    * @param env_info
    */
  override def env_init(env_info: EnvInfo): Unit = {
    Random.setSeed(env_info.seed)
    range_points = (-5,5)
    valid_actions = Array.range(0,nodesToTry*2+1)
    actions = Array((0,0))++Array.range(0,nodesToTry).flatMap(i => Array((i,-1),(i,1)))
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
  val observation = (1 to nodesToTry).map(_ => Random.nextInt(11)-5d).toArray
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
  val (indx,turn) = actions(action)
  val last_state = reward_obs_term._2.clone()
  /*
  if((indx until last_state.length).forall{ i =>
    val nv = last_state(i)+turn*0.5d
    nv>=range_points._1 && nv <=range_points._2
  }) {
    (indx until last_state.length).foreach { i =>
      last_state(i) += turn * 0.5d
    }
  }

   */
  val nv = last_state(indx) + turn*0.5d
  val reward = if(nv>=range_points._1 && nv <= range_points._2){
    val pv = last_state(indx)

    last_state(indx) += turn*0.5d
    pv*pv-nv*nv
  }else {
    last_state.indices.foreach{ i =>
      last_state(i) = Random.nextInt(11)-5d
    }

    -25d
  }
  last_action = action


  //val reward = -(last_state.map(x => x * x).sum)
  val is_terminal = false
  reward_obs_term = (reward.toFloat,last_state,is_terminal)
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
