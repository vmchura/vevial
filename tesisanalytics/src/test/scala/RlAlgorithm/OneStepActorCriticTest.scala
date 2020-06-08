package RlAlgorithm

import org.scalatest.flatspec.AnyFlatSpec

class OneStepActorCriticTest extends AnyFlatSpec {

  behavior of "OneStepActorCritic"
  it  should "not give any errors" in {
    //(iht_size = 4096,num_tilings = 8, num_tiles = 8, actor_step_size = 1e-1f,
    //      critic_step_size = 1e-0f, avg_reward_step_size = 1e-2f, num_actions = 3, seed = 99,0.9f)
    val maxEpisodes = 10000
    val rewardsAfterEpisode = OneStepActorCritic.runExperimentLinearStrip(4096,8,8,0.9f,1e-1f,1e-0f,maxEpisodes)
    assertResult(maxEpisodes)(rewardsAfterEpisode.size)
    assert(rewardsAfterEpisode.lastOption.fold(false)(_ >= 9d))
  }
}
