package CommunicationLayerPython

import java.util.UUID

import upickle.default.{ReadWriter => RW, macroRW}


trait Message

sealed trait Request extends Message

object Request{
  implicit val rw: RW[Request] = macroRW

}
sealed trait Response extends Message


case class ActionsForState(actions: Int)

case class Regard(value: Double)


//Initial Welcome
case class Welcome(message: String) extends Response
case class Farewell(message: String) extends Response


//********************* REQUEST ********************************


/**
  * Asks for a new experiment
  * @param experiment: Experiment TAG
  */
case class NewExperiment(experiment: String) extends Request
/**
  * Alter the experiment with action
  * @param experimentID
  * @param action: index of the action
  */
case class Action(experimentID: UUID, action: Int) extends Request

/**
  * Request to end the experimentID
  * @param experimentID
  */
case class EndExperiment(experimentID: UUID) extends Request




//********************* RESPONSE ********************************

case class InvalidRequest(error: String) extends Response

/**
  *
  * @param experimentID
  * @param actionsForState, the initial actions for the initia  state
  */
case class ExperimentResp(experimentID: UUID,
                      actionsForState: ActionsForState) extends Response

/**
  * the enviroment is updated, only these new actions can be done
  * @param actionsForNewState
  * @param regard: the regard for the previous action
  */
case class NewState(actionsForNewState: ActionsForState,
                    regard: Regard) extends Response

object NewExperiment{
  implicit val rw: RW[NewExperiment] = macroRW
}
object Action{
  implicit val rw: RW[Action] = macroRW
}
object Regard{
  implicit val rw: RW[Regard] = macroRW
}
object ActionsForState{
  implicit val rw: RW[ActionsForState] = macroRW
}
object ExperimentResp{
  implicit val rw: RW[ExperimentResp] = macroRW
}
object NewState{
  implicit val rw: RW[NewState] = macroRW
}
object InvalidRequest{
  implicit val rw: RW[InvalidRequest] = macroRW
}
object Welcome{
  implicit val rw: RW[Welcome] = macroRW
}
object Farewell{
  implicit val rw: RW[Farewell] = macroRW
}
object EndExperiment{
  implicit val rw: RW[EndExperiment] = macroRW
}