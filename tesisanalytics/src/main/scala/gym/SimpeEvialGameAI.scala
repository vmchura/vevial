package gym
import java.util.UUID

import CommunicationLayerPython.Experiment
import gym.evialgame.EvialGame

class SimpeEvialGameAI(evialGame: EvialGame) extends EvialGameAI {
  override def maxSteps: Int = 1000

  override def solve(): EvialGame = {
    val evialGameExperiment: Experiment = EvialGameExperiment(UUID.randomUUID(),evialGame,evialGame)
    val result = (1 to maxSteps).foldLeft(evialGameExperiment)((prevGame, _ ) => {
      if(prevGame.finished)
        prevGame
      else {
        val actionToTake = (0 until prevGame.actions).map { action =>
          val regard = prevGame.regard(action)
          (regard, action)
        }.maxBy(_._1)._2
        prevGame.update(actionToTake)
      }
    })

    result match {
      case EvialGameExperiment(_,_,resultGame) => resultGame
      case _ => {
        println("ERROR")
        evialGame
      }
    }
  }
}
