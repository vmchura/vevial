package environment
import environment.BaseEnvironmentTyped._
import TerminalState._
/**
  * |o|....|f|
  */
class LinearEnvironmentTyped() extends BaseEnvironmentTyped[Int]{
  private val valid_actions:  Array[Action] = Array.range(0,3)
  private val actions: Array[Int] = Array(-1,0,1)
  private val END_STRIP: Int = 10
  private var currentState = -1
  private var environmentFinished: TerminalState.Value = UnknowState
  /**
    * The first method called when the experiment starts, called before the
    * agent starts.
    *
    * @return The first state observation from the environment.
    */
  override def env_start(): Int = {
    currentState = 0
    environmentFinished = OnProcess
    stateToObservation(currentState)
  }
  private def stateToObservation(state: Int): Int = state

  /**
    * A step taken by the environment.
    *
    * @param action The action taken by the agent
    * @return (float, state, Boolean): a tuple of the reward, state observation,
    *         and boolean indicating if it's terminal.
    */
  override def env_step(action: Action): Either[EnvironmentError,RewardFeatureState[Int]] = {

    def processAction(): Either[EnvironmentError,RewardFeatureState[Int]] = {

      val indexAction = valid_actions.indexWhere(_ == action)
      if(indexAction >= 0){
        (actions(indexAction),currentState) match {
          case (_,s) if s < 0 || s > END_STRIP => Left(StateIsUnknow(s"State: $currentState is not correctly defined"))
          case (_,END_STRIP) => Left(StateIsUnknow(s"State is $currentState, and END_STRIP is $END_STRIP"))
          case (-1,0) => Right(RewardFeatureState(-1f,stateToObservation(0),OnProcess))
          case (a,_) =>
            currentState += a
            val isTerminal = if(currentState == END_STRIP) Finished else OnProcess
            environmentFinished = isTerminal
            Right(RewardFeatureState(if(a==1) 1f else -1f, stateToObservation(currentState), isTerminal))
        }
      }else{
        Left(InvalidAction(s"Action: $action is invalid"))
      }
    }

    environmentFinished match {
      case OnProcess => processAction()
      case UnknowState => Left(StateIsUnknow("State not initilized"))
      case Finished => Left(StateIsTerminal)
    }

  }

}
