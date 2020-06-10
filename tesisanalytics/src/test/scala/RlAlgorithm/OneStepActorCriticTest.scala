package RlAlgorithm

import org.scalatest.flatspec.AnyFlatSpec

class OneStepActorCriticTest extends AnyFlatSpec {

  behavior of "OneStepActorCritic"
  it  should "not give any errors" in {
    //(iht_size = 4096,num_tilings = 8, num_tiles = 8, actor_step_size = 1e-1f,
    //      critic_step_size = 1e-0f, avg_reward_step_size = 1e-2f, num_actions = 3, seed = 99,0.9f)
    val maxEpisodes = 10000
    val rewardsAfterEpisode = OneStepActorCritic.runExperimentActorCriticSoftMax(4096,8,8,0.9f,1e-1f,1e-0f,maxEpisodes,OneStepActorCritic.LINEAR_STRIP_COMPONENT_BUILDER)
    assertResult(maxEpisodes)(rewardsAfterEpisode.size)
    assert(rewardsAfterEpisode.lastOption.fold(false)(_ >= 9d))
  }
  it  should "not give any errors on Fantasy" in {
    //(iht_size = 4096,num_tilings = 8, num_tiles = 8, actor_step_size = 1e-1f,
    //      critic_step_size = 1e-0f, avg_reward_step_size = 1e-2f, num_actions = 3, seed = 99,0.9f)
    val maxEpisodes = 10000
    val rewardsAfterEpisode = OneStepActorCritic.runExperimentActorCriticSoftMax(4096,
      4,
      4,
      0.7f,
      10f,
      10f,maxEpisodes,OneStepActorCritic.FANTASY_PARABOLIC_COMPONENT_BUILDER)
    assertResult(maxEpisodes)(rewardsAfterEpisode.size)
    println(rewardsAfterEpisode)
    //assert(rewardsAfterEpisode.lastOption.fold(false)(_ >= 9d))
  }
}
