package gym

import java.util.UUID

import CommunicationLayerPython.Experiment
import com.typesafe.scalalogging.Logger

/**
  * Simple Experiment
  * in a 10 line row
  * action to go left:-2, stay: -1
  * action to go right: +1
  * action to go left and being in position 1: done regard: -100
  * finised when position is 10
  */
case class SimpleExperiment(override val id: UUID, position: Int = 1, finished: Boolean = false, stepsMade: Int = 0) extends Experiment{
  val logger = Logger[SimpleExperiment]

  override def reset: Experiment = {
    logger.info(s"Reset after $stepsMade steps, in position: $position")
    SimpleExperiment(id)
  }

  override def update(action: Int): Experiment = {
     if(!finished) {

       val updated = (action match {
        case 0 => if(position == 1) copy(finished = true) else copy(position = position-1)
        case 1 => this
        case 2 => copy(position = position +1, finished = position == 9)
      }).copy(stepsMade = stepsMade +1)

       if(updated.stepsMade >= 20)
         updated.copy(finished = true)
       else
         updated

    } else
      this


  }


  override def regard(action: Int): Double =
    if(!finished)
      action match {
        case 0 => if(position == 1) -100 else -2
        case 1 => -1
        case 2 => 1
        case _ => throw new IllegalArgumentException()
    }else{
      0
    }

  override def actions: Int = if(finished) 0 else 3

  override val dimState: Int = 1

  override def state: Array[Double] = Array(position)
}
