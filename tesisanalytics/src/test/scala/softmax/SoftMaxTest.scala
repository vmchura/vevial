package softmax

import agent.{ActorCriticSoftmaxAgent, ActorCriticSoftmaxAgentLinear, AgentInfo}
import environment.{EnvInfo, LinearEnvironment, PendulumEnvironment}
import org.scalatest.flatspec.AnyFlatSpec
import rlglue.RLGlue
import softmax.SoftMax.{AgentInforExperiment, ExperimentParamenters}
import tiles.PendulumTileCoder

class SoftMaxTest extends AnyFlatSpec {
  behavior of "SoftMax  helper functions"
  it should "give the correct expected value" in {
    val iht_size = 4096
    val num_tilings = 8
    val num_tiles = 8
    val test_tc = new PendulumTileCoder(iht_size=iht_size,num_tilings=num_tilings,num_tiles=num_tiles)

    val num_actions = 3
    val actions = (0 until num_actions).toList
    val actor_w = Array.fill(num_actions,iht_size)(0d)

    Seq(-1d,1d,2d).zipWithIndex.foreach{
      case (v,indx) => actor_w(indx).indices.foreach{ j =>
        actor_w(indx)(j) = v/num_tilings
      }
    }

    val state = (-Math.PI.toFloat,0f)
    val (angle,ang_vel) = state
    val active_tiles = test_tc.get_tiles(angle,ang_vel)

    val softmax_prob: Array[Double] = SoftMax.compute_softmax_prob(actor_w,active_tiles)
    val expected = Array(0.03511903,0.25949646,0.70538451)
    softmax_prob.zip(expected).foreach{
      case (a,b) => assert(Math.abs(b-a)<1e-6)
    }

  }

  it should "work fine with glue and environment" in {
    val envInfo = EnvInfo(seed = 99)
    val agentInfo = AgentInfo(iht_size = 4096,num_tilings = 8, num_tiles = 8, actor_step_size = 1e-1f,
      critic_step_size = 1e-0f,avg_reward_step_size = 1e-2f, num_actions = 3, seed = 99,0.9f)

    val test_env = () => new PendulumEnvironment
    val test_agent = () => new ActorCriticSoftmaxAgent

    val rl_glue = new RLGlue(test_env,test_agent)
    rl_glue.rl_init(agentInfo,envInfo)

    rl_glue.rl_start()
    rl_glue.rl_step()
    println(rl_glue.agent)

  }
/*
  it should "generate correct runs" in {
    val experimentParamenters = ExperimentParamenters(50,20000)
    val envParamenters = EnvInfo(99)
    val agentParamenters = AgentInforExperiment(num_tilings=Seq(32),num_tiles = Seq(8),
      actor_step_size = Seq(0.25f),critic_step_size = Seq(2),avg_reward_step_size =Seq(0.015625f),num_actions = 3, iht_size = 4096 )

    SoftMax.run_experiment(() => new PendulumEnvironment,() => new ActorCriticSoftmaxAgent,envParamenters,agentParamenters,experimentParamenters)
  }

 */
  it should "generate correct runs" in {
    val experimentParamenters = ExperimentParamenters(50,20000)
    val envParamenters = EnvInfo(99)
    val agentParamenters = AgentInforExperiment(num_tilings=Seq(32,64),num_tiles = Seq(8,16),
      actor_step_size = Seq(0.25f),critic_step_size = Seq(2),avg_reward_step_size =Seq(0.015625f),num_actions = 3, iht_size = 4096 )

    SoftMax.run_experiment(() => new LinearEnvironment,() => new ActorCriticSoftmaxAgentLinear,envParamenters,agentParamenters,experimentParamenters)
  }
}
