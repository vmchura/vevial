package softmax

import agent.{ActorCriticSoftmaxAgent, ActorCriticSoftmaxAgentBreeze, ActorCriticSoftmaxAgentLinear, AgentInfo, CuerdaAgent, RopeAgent}
import environment.{CuerdaEnvironment, EnvInfo, LinearEnvironment, PendulumEnvironment, RopeEjeEnvironment}
import org.scalatest.flatspec.AnyFlatSpec
import rlglue.RLGlue
import tiles.PendulumTileCoder
import breeze.linalg._

class SoftMaxBreezeTest extends AnyFlatSpec {
  behavior of "SoftMax BREEZE helper functions"
  it should "give the correct expected value" in {
    val iht_size = 4096
    val num_tilings = 8
    val num_tiles = 8
    val test_tc = new PendulumTileCoder(iht_size=iht_size,num_tilings=num_tilings,num_tiles=num_tiles)

    val num_actions = 3
    val actions = (0 until num_actions).toList
    val actor_w: DenseMatrix[Double] = DenseMatrix.zeros(num_actions,iht_size)

    Seq(-1d,1d,2d).zipWithIndex.foreach{
      case (v,indx) => {
        actor_w(indx,::) += v/num_tilings

      }
    }

    val state = (-Math.PI.toFloat,0f)
    val (angle,ang_vel) = state
    val active_tiles = test_tc.get_tiles(angle,ang_vel)

    val softmax_prob: Vector[Double] = SoftMaxBreeze.compute_softmax_prob(actor_w,active_tiles)
    val expected: Vector[Double] = Vector(0.03511903,0.25949646,0.70538451)
    (expected-softmax_prob).foreach(d => assert(Math.abs(d)<1e-6))


  }

  it should "work fine with glue and environment episodic" in {
    val envInfo = EnvInfo(seed = 99)
    val agentInfo = AgentInfo(iht_size = 4096,num_tilings = 8, num_tiles = 8, actor_step_size = 1e-1f,
      critic_step_size = 1e-0f,avg_reward_step_size = 1e-2f, num_actions = 2, seed = 99, gamma = 0.9f)

    val test_env = () => new LinearEnvironment
    val test_agent = () => new ActorCriticSoftmaxAgentLinear

    val rl_glue = new RLGlue(test_env,test_agent)
    rl_glue.rl_init(agentInfo,envInfo)

    rl_glue.rl_start()
    rl_glue.rl_step()
    println(rl_glue.agent)

  }

  it should "generate correct runs  CUERDA " in {
    import SoftMaxBreeze._

    val experimentParamenters = ExperimentParamenters(5,2000000)
    val envParamenters = EnvInfo(99)
    val agentParamenters = AgentInforExperiment(num_tilings=Seq(64),num_tiles = Seq(8),
      actor_step_size = Seq(2f),critic_step_size = Seq(0.5f),avg_reward_step_size =
        Seq(0.15625f),num_actions = 21, iht_size = 4096*64*8,gamma = Seq(0.8f) )

    SoftMaxBreeze.run_experiment(() => new CuerdaEnvironment,
      () => new CuerdaAgent,envParamenters,
      agentParamenters,experimentParamenters)
  }

  it should "generate correct runs  ROPE " in {
    import SoftMaxBreeze._

    val experimentParamenters = ExperimentParamenters(5,2000000)
    val envParamenters = EnvInfo(99)
    val agentParamenters = AgentInforExperiment(num_tilings=Seq(64),num_tiles = Seq(8),
      actor_step_size = Seq(2f),critic_step_size = Seq(0.5f),avg_reward_step_size =
        Seq(0.15625f),num_actions = 21, iht_size = 4096*64*8,gamma = Seq(0.8f) )

    SoftMaxBreeze.run_experiment(() => new RopeEjeEnvironment,
      () => new RopeAgent,envParamenters,
      agentParamenters,experimentParamenters)
  }


  /*
  it should "generate correct runs" in {
    val experimentParamenters = ExperimentParamenters(50,20000)
    val envParamenters = EnvInfo(99)
    val agentParamenters = AgentInforExperiment(num_tilings=Seq(32,64),num_tiles = Seq(8,16),
      actor_step_size = Seq(0.25f),critic_step_size = Seq(2),avg_reward_step_size =Seq(0.015625f),num_actions = 3, iht_size = 4096 )

    SoftMax.run_experiment(() => new LinearEnvironment,() => new ActorCriticSoftmaxAgentLinear,envParamenters,agentParamenters,experimentParamenters)
  }

   */
}
