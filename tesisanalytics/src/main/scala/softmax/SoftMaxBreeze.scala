package softmax

import RFunctionDefiner.{PlotRLEvolution, RInitializer}
import agent.{AgentInfo, BaseAgent}
import environment.{BaseEnvironment, EnvInfo}
import rlglue.RLGlue
import breeze.linalg._

object SoftMaxBreeze {
  import RFunctionDefiner.RInitializer.R




  /**
    * Computes softmax probability for all actions
    * @param actor_w    np.array, an array of actor weights
    * @param tiles      np.array, an array of active tiles
    * @return           np.array, an array of size equal to num. actions, and sums to 1.
    */
  def compute_softmax_prob(actor_w: DenseMatrix[Double], tiles: Array[Int]): Vector[Double] = {

    /**
      * First compute the list of state-action preferences (1~2 lines)
      * state_action_preferences = ? (list of size 3)
      */
    // h(s,a): Vector[Double] = tiles
    //                                                            𝑇
    //val state_action_preferences: Vector[Double] = ℎ(𝑠,𝑎,𝜃) = 𝜃   ℎ(𝑠,𝑎)
    val state_action_preferences: Vector[Double] = sum(actor_w(::,tiles.toSeq),Axis._1)

    val ans = SoftmaxTyped.compute_softmax_prob(state_action_preferences)

    if(ans(0).isNaN){
      val s = s"""
                 |actor_w : $actor_w
                 |tiles: ${tiles.mkString(" - ")}
                 |sliced: ${actor_w(::,tiles.toSeq).toString(maxLines = 10,maxWidth = 5000)}
                 |state_action: $state_action_preferences

.                 |""".stripMargin
      println(s)
    }
    ans

  }

  final def sample(dist: Vector[Double]): Int = {
    val p = scala.util.Random.nextDouble
    val it = dist.iterator
    var accum = 0.0
    var indx = -1
    var i = 0
    while (it.hasNext) {
      val (_,itemProb): (Int, Double) = it.next
      accum += itemProb
      if (accum >= p && indx == -1)
        indx = i
      i += 1// return so that we don't have to search through the whole distribution
    }
    if(indx == -1) {
      println(dist)
      throw  new IllegalArgumentException("is not a distribution")
    }
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
        avg_reward_step_size =avg_reward_ss,num_actions = agentInfoExperiment.num_actions,99,gamma = 0.9f
      )

      //val return_per_step = Array.fill(experimentParameter.num_runs,experimentParameter.max_steps)(0d)
      //val exp_avg_reward_per_step = Array.fill(experimentParameter.num_runs,experimentParameter.max_steps)(0d)
      val runs: Array[(Int,Double, Double)] = (1 to experimentParameter.num_runs).flatMap{ run =>
        val envInfoRun = envInfo.copy(seed = run)
        val agentInfoRun = agentInfo.copy(seed = run)

        rl_glue.rl_init(agentInfoRun,envInfoRun)
        rl_glue.rl_start()

        val exp_avg_reward_ss = 0.01



        var lastResult: Array[Double] = null
        val totReturnAvgReward = (1 to experimentParameter.max_steps).scanLeft((0,0d,0d,0d,0d)){ case ((_,total_return, exp_avg_reward,exp_avg_reward_normalizer,_),num_steps) =>
          val rl_step_result = rl_glue.rl_step()
          val reward_next = rl_step_result._1
          lastResult = rl_step_result._2
          val exp_avg_reward_normalizer_next = exp_avg_reward_normalizer + exp_avg_reward_ss* (1 - exp_avg_reward_normalizer)
          val ss = exp_avg_reward_ss/exp_avg_reward_normalizer_next
          val exp_avg_reward_next = exp_avg_reward + ss * (reward_next-exp_avg_reward)
          (num_steps,total_return+reward_next,exp_avg_reward_next,exp_avg_reward_normalizer_next,reward_next)

        }.tail.map(c => (c._1,c._5,c._3))

        println(lastResult.mkString(", "))
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

      //PlotRLEvolution.drawEvolutionRL(runs.map(_._1),runs.map(_._2),"Total return","steps","total return",s"/home/vmchura/Documents/CUERDA-RUN$num_tilings-$num_tiles-$critic_ss.png")
      //PlotRLEvolution.drawEvolutionRL(runs.map(_._1),runs.map(_._3),"Avg reward","steps","avg reward",s"/home/vmchura/Documents/CUERDA-AVR$num_tilings-$num_tiles-$critic_ss.png")

    }
  }


  def run_experiment_episodic(environment: () => BaseEnvironment, agent:() => BaseAgent, envInfo: EnvInfo, agentInfoExperiment: AgentInforExperiment, experimentParameter: ExperimentParamenters): Unit ={
    RInitializer.init()

    val rl_glue = new RLGlue(environment,agent)
    for{
      num_tilings <-  agentInfoExperiment.num_tilings
      num_tiles <- agentInfoExperiment.num_tiles
      actor_ss <- agentInfoExperiment.actor_step_size
      critic_ss <- agentInfoExperiment.critic_step_size
      avg_reward_ss <- agentInfoExperiment.avg_reward_step_size
      gamma <- agentInfoExperiment.gamma
    }yield{

      println(s"num_tilings: $num_tilings num_tiles: $num_tiles actor_ss: $actor_ss critic_ss: $critic_ss avg_reward_ss: $avg_reward_ss")
      val envInfo = EnvInfo(1)

      val agentInfo = AgentInfo(iht_size=agentInfoExperiment.iht_size,num_tilings = num_tilings,
        num_tiles = num_tiles,actor_step_size = actor_ss,critic_step_size = critic_ss,
        avg_reward_step_size =avg_reward_ss,num_actions = agentInfoExperiment.num_actions,99,gamma
      )

      //val return_per_step = Array.fill(experimentParameter.num_runs,experimentParameter.max_steps)(0d)
      //val exp_avg_reward_per_step = Array.fill(experimentParameter.num_runs,experimentParameter.max_steps)(0d)
      val runs: Array[(Int, Double)] = (1 to experimentParameter.num_runs).flatMap{ run =>
        val envInfoRun = envInfo.copy(seed = run)
        val agentInfoRun = agentInfo.copy(seed = run)

        rl_glue.rl_init(agentInfoRun,envInfoRun)

        (1 to experimentParameter.max_steps).map{ iter =>
          rl_glue.rl_start()
          var isTerminal = false
          while(!isTerminal){
            val (_,_,_,isTerminalRes) = rl_glue.rl_step()
            isTerminal = isTerminalRes
          }

          //rl_glue.rl_start()
          (iter,rl_glue.agent.real_value(rl_glue.env.env_start()))

        }

      }.toArray

      PlotRLEvolution.drawEvolutionRL(runs.map(_._1),runs.map(_._2),"Evolution episodic","steps","total return",s"/home/vmchura/Documents/EV-Episodic$num_tilings-$num_tiles-$critic_ss.png")

    }
  }


  case class ExperimentParamenters(num_runs: Int, max_steps: Int)

  case class AgentInforExperiment(num_tilings: Seq[Int], num_tiles: Seq[Int], actor_step_size: Seq[Float],
                                  critic_step_size: Seq[Float],
                                  avg_reward_step_size: Seq[Float],gamma: Seq[Float], num_actions: Int, iht_size: Int)

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
