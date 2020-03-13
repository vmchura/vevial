package rlglue

import agent.{AgentInfo, BaseAgent}
import environment.{BaseEnvironment, EnvInfo}

class RLGlue(env_class: () => BaseEnvironment, agent_class: () => BaseAgent) {

  var total_reward = 0d
  var last_action = -1
  var num_steps = -1
  var num_episodes = -1

  val env: BaseEnvironment = env_class()
  val agent: BaseAgent = agent_class()
  def rl_init(agent_init_info: AgentInfo, env_init_info: EnvInfo): Unit = {
    env.env_init(env_info = env_init_info)
    agent.agent_init(agent_init_info)

    total_reward = 0d
    num_steps = 0
    num_episodes = 0

  }

  def rl_start(): (Array[Double],Int) = {
    total_reward = 0d
    num_steps = 1
    val last_state = env.env_start()
    last_action = agent.agent_start(last_state)

    val observation = (last_state,last_action)
    observation
  }

  def rl_agent_start(observation: Array[Double]): Int = agent.agent_start(observation)

  def rl_agent_end(reward: Float): Unit = agent.agent_end(reward)

  def rl_env_start(): Array[Double] = {
    total_reward = 0d
    num_steps = 1
    env.env_start()
  }

  def rl_env_step(action: Int) :  (Float, Array[Double], Boolean) = {
    val ro = env.env_step(action)
    val (this_reward,_,terminal) = ro
    total_reward += this_reward
    if(terminal)
      num_episodes += 1
    else{
      num_steps += 1
    }

    ro
  }

  def rl_step(): (Float, Array[Double],Int, Boolean) = {
    val (reward,last_state,term) = env.env_step(last_action)

    total_reward += reward

    val roat = if(term){
      num_episodes += 1
      agent.agent_end(reward)
      (reward,last_state,-1,term)
    }else{
      num_steps += 1
      last_action = agent.agent_step(reward,last_state)
      (reward,last_state,last_action,term)
    }

    roat
  }

  def rl_cleanup(): Unit = {
    env.env_cleanup()
    agent.agent_cleanup()
  }

  def rl_agent_message(message: String): Double = agent.agent_message(message)
  def rl_env_message(message: String): Double = env.env_message(message)

  def rl_episode(max_steps_this_episode: Int): Boolean = {
    var is_terminal = false
    rl_start()
    while((!is_terminal) && ((max_steps_this_episode == 0) || (num_steps < max_steps_this_episode))){
      val rl_step_result = rl_step()
      is_terminal = rl_step_result._4
    }

    is_terminal
  }

  def rl_return(): Double = total_reward
  def rl_num_steps(): Int = num_episodes



}
