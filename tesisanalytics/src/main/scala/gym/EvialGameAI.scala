package gym

import gym.evialgame.EvialGame

trait EvialGameAI {
  def maxSteps: Int
  def solve(): EvialGame
}
