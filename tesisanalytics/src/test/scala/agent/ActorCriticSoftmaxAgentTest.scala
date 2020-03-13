package agent

import org.scalatest.flatspec.AnyFlatSpec

class ActorCriticSoftmaxAgentTest extends AnyFlatSpec {

  behavior of "Pendulum Agent"
  it should "Run ok first templates" in {
    val agentInfo = AgentInfo(iht_size = 4096,num_tilings = 8, num_tiles = 8, actor_step_size = 1e-1f,
      critic_step_size = 1e-0f, avg_reward_step_size = 1e-2f, num_actions = 3, seed = 99)

    val test_agent = new ActorCriticSoftmaxAgent()
    test_agent.agent_init(agentInfo)
    val state = Array(-Math.PI,0d)
    test_agent.agent_start(state)
    println(s"agent active_tiles: [${test_agent.prev_tiles.mkString(" ")}]")
    println(s"agent selected_action : [${test_agent.last_action}]")
  }
}
