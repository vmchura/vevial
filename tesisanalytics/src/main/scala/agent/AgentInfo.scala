package agent

case class AgentInfo(iht_size: Int, num_tilings: Int,
                     num_tiles: Int, actor_step_size: Float,
                     critic_step_size: Float, avg_reward_step_size: Float,
                     num_actions: Int, seed: Int)
