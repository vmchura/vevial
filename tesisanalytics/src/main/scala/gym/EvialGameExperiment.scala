package gym

import java.util.UUID

import CommunicationLayerPython.Experiment
import gym.evialgame._

case class EvialGameExperiment(id: UUID,initlaEvialGame: EvialGame, currentEvialGame: EvialGame) extends Experiment {
  override def reset: Experiment = copy(currentEvialGame = initlaEvialGame)

  override def actions: Int = 6

  private def vialGameAfterAction(action: Int): EvialGame = action match {
    case 0 => currentEvialGame.forward()
    case 1 => currentEvialGame.backward()
    case 2 => currentEvialGame.turn(1d)
    case 3 => currentEvialGame.turn(10d)
    case 4 => currentEvialGame.turn(-1d)
    case 5 => currentEvialGame.turn(-10d)
    case _ => currentEvialGame
  }

  override def update(action: Int): Experiment = copy(currentEvialGame = vialGameAfterAction(action))


  override def regard(action: Int): Double = {
    val gafter = vialGameAfterAction(action)
    -gafter.currentScore
  }

  override def finished: Boolean = currentEvialGame.gameDone

  override def dimState: Int = 1+3*EvialGame.SAMPLES_TO_DEFINE_STATE

  override def state: Array[Double] = {
    if(finished)
      Array.fill(dimState)(0d)
    else {
      val listElements: Array[Double] = currentEvialGame.sampleState().flatMap {
        case SampleVector(prog,distance,_) =>Array(prog.toDouble,distance,distance*distance)
        case ExactVector(prog) =>Array(prog.toDouble,0d,0d)
        case UndefinedSample => Array(0d,0d,0d)
      }.toArray

      Array(currentEvialGame.currentState.toDouble) ++ listElements
    }
  }
}
