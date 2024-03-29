package softmax

import RFunctionDefiner.{PlotRLEvolution, RInitializer}
import agent.{AgentInfo, BaseAgent}
import environment.{BaseEnvironment, EnvInfo}
import rlglue.RLGlue


object SoftMax {
  import RFunctionDefiner.RInitializer.R
  /**
    * Computes softmax probability for all actions
    * @param actor_w    np.array, an array of actor weights
    * @param tiles      np.array, an array of active tiles
    * @return           np.array, an array of size equal to num. actions, and sums to 1.
    */
  def compute_softmax_prob(actor_w: Array[Array[Double]], tiles: Array[Int]): Array[Double] = {

    /**
      * First compute the list of state-action preferences (1~2 lines)
      * state_action_preferences = ? (list of size 3)
      */
    //state_action_preferences = actor_w[:, tiles].sum(axis=1)
    val state_action_preferences: Array[Double] = actor_w.map(w => tiles.map(w).sum)

    /**
      * # Set the constant c by finding the maximum of state-action preferences (use np.max) (1 line)
      * # c = ? (float)
      * ### START CODE HERE ###
      * c = np.max(state_action_preferences)
      * ### END CODE HERE ###
      */
    val c = state_action_preferences.max

    /**
      * # Compute the numerator by subtracting c from state-action preferences and exponentiating it (use np.exp) (1 line)
      * # numerator = ? (list of size 3)
      * ### START CODE HERE ###
      * numerator = np.exp(state_action_preferences - c)
      * ### END CODE HERE ###
      */

    val numerator = state_action_preferences.map{p => Math.exp(p-c)}

    /**
      * # Next compute the denominator by summing the values in the numerator (use np.sum) (1 line)
      * # denominator = ? (float)
      * ### START CODE HERE ###
      * denominator = np.sum(numerator)
      * ### END CODE HERE ###
      */

    val denominator = numerator.sum

    /**
      * # Create a probability array by dividing each element in numerator array by denominator (1 line)
      * # We will store this probability array in self.softmax_prob as it will be useful later when updating the Actor
      * # softmax_prob = ? (list of size 3)
      * ### START CODE HERE ###
      * softmax_prob = numerator / denominator
      * ### END CODE HERE ###
      */
    val softmax_prob = numerator.map(_ / denominator)

    softmax_prob
  }

  final def sample(dist: Seq[Double]): Int = {
    val p = scala.util.Random.nextDouble
    val it = dist.iterator
    var accum = 0.0
    var indx = -1
    var i = 0
    while (it.hasNext) {
      val (itemProb) = it.next
      accum += itemProb
      if (accum >= p && indx == -1)
        indx = i
      i += 1// return so that we don't have to search through the whole distribution
    }
    if(indx == -1)
      throw  new IllegalArgumentException("is not a distribution")
    indx
  }

  def run_experiment(environment: () => BaseEnvironment, agent:() => BaseAgent, envInfo: EnvInfo, agentInfoExperiment: AgentInforExperiment, experimentParameter: ExperimentParamenters): Unit ={
    RInitializer.init()

    val rl_glue = new RLGlue(environment,agent)
    for{
      num_tilings <-  agentInfoExperiment.num_tilings
      num_tiles <- agentInfoExperiment.num_tiles
      actor_ss <- agentInfoExperiment.actor_step_size
      critic_ss <- agentInfoExperiment.critic_step_size
      avg_reward_ss <- agentInfoExperiment.avg_reward_step_size
    }yield{

      println(s"num_tilings: $num_tilings num_tiles: $num_tiles actor_ss: $actor_ss critic_ss: $critic_ss avg_reward_ss: $avg_reward_ss")
      val envInfo = EnvInfo(1)

      val agentInfo = AgentInfo(iht_size=agentInfoExperiment.iht_size,num_tilings = num_tilings,
        num_tiles = num_tiles,actor_step_size = actor_ss,critic_step_size = critic_ss,
        avg_reward_step_size =avg_reward_ss,num_actions = agentInfoExperiment.num_actions,99,0.9f
      )

      //val return_per_step = Array.fill(experimentParameter.num_runs,experimentParameter.max_steps)(0d)
      //val exp_avg_reward_per_step = Array.fill(experimentParameter.num_runs,experimentParameter.max_steps)(0d)
      val runs: Array[(Int,Double, Double)] = (1 to experimentParameter.num_runs).flatMap{ run =>
        val envInfoRun = envInfo.copy(seed = run)
        val agentInfoRun = agentInfo.copy(seed = run)

        rl_glue.rl_init(agentInfoRun,envInfoRun)
        rl_glue.rl_start()

        val exp_avg_reward_ss = 0.01



        val totReturnAvgReward = (1 to experimentParameter.max_steps).scanLeft((0,0d,0d,0d)){ case ((_,total_return, exp_avg_reward,exp_avg_reward_normalizer),num_steps) =>
          val rl_step_result = rl_glue.rl_step()
          val reward_next = rl_step_result._1
          val exp_avg_reward_normalizer_next = exp_avg_reward_normalizer + exp_avg_reward_ss* (1 - exp_avg_reward_normalizer)
          val ss = exp_avg_reward_ss/exp_avg_reward_normalizer_next
          val exp_avg_reward_next = exp_avg_reward + ss * (reward_next-exp_avg_reward)
          (num_steps,total_return+reward_next,exp_avg_reward_next,exp_avg_reward_normalizer_next)

        }.tail.map(c => (c._1,c._2,c._3))

        totReturnAvgReward

        /*
        var num_steps = 0
        var total_return = 0d

        var exp_avg_reward = 0d
        var exp_avg_reward_normalizer = 0d
        while( num_steps < experimentParameter.max_steps){
          num_steps += 1
          val rl_step_result = rl_glue.rl_step()
          val reward = rl_step_result._1
          total_return += reward

          val avg_reward = rl_glue.rl_agent_message("get avg reward")

          exp_avg_reward_normalizer = exp_avg_reward_normalizer + exp_avg_reward_ss* (1 - exp_avg_reward_normalizer)
          val ss = exp_avg_reward_ss/exp_avg_reward_normalizer
          exp_avg_reward += ss * (reward-exp_avg_reward)
          if(num_steps % 1000 == 0)
            print(f"$exp_avg_reward%6.3f ")

        }
        */




      }.toArray

      PlotRLEvolution.drawEvolutionRL(runs.map(_._1),runs.map(_._2),"Total return","steps","total return",s"/home/vmchura/Documents/TR$num_tilings-$num_tiles-$critic_ss.png")
      PlotRLEvolution.drawEvolutionRL(runs.map(_._1),runs.map(_._3),"Avg reward","steps","avg reward",s"/home/vmchura/Documents/AVR$num_tilings-$num_tiles-$critic_ss.png")

    }
  }


  case class ExperimentParamenters(num_runs: Int, max_steps: Int)

  case class AgentInforExperiment(num_tilings: Seq[Int], num_tiles: Seq[Int], actor_step_size: Seq[Float], critic_step_size: Seq[Int], avg_reward_step_size: Seq[Float], num_actions: Int, iht_size: Int)

  def normalizeVariable(delta: Double)(actualValue: Double): Double = {
    val range = delta*2d
    val res = actualValue % range
    if(res.sign>0){
        if(res > range-res){
            -res
        }else{
          range-res
        }
    }else{
      if(res > range-res){
        -range+res
      }else{
        res
      }
    }

  }
}
