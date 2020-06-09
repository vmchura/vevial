package environment

import environment.BaseEnvironmentTyped.{Action, EnvironmentError, InvalidAction, RewardFeatureState, StateIsTerminal, StateIsUnknow, TerminalState}
import environment.BaseEnvironmentTyped.TerminalState.{Finished, OnProcess, UnknowState}
import gym.{FantasyParabolicData, FantasyParabolicProblem}

class EnvironmentACFantasyParabolic extends BaseEnvironmentTyped[FantasyParabolicProblem]{
  private val valid_actions:  Array[Action] = Array.range(0,4)
  private val actions: Array[Int] = Array.range(0,4)
  private var currentState: FantasyParabolicProblem = new FantasyParabolicProblem(Nil,0,0)
  private var environmentFinished: TerminalState.Value = UnknowState

  /**
    * The first method called when the experiment starts, called before the
    * agent starts.
    *
    * @return The first state observation from the environment.
    */
  override def env_start(): FantasyParabolicProblem = {
    val data = List(FantasyParabolicData(0,0),FantasyParabolicData(100,0),FantasyParabolicData(200,2.1f),FantasyParabolicData(300,3f))
    currentState = new FantasyParabolicProblem(data,0,400)
    environmentFinished = OnProcess
    currentState
  }

  /**
    * A step taken by the environment.
    *
    * @param action The action taken by the agent
    * @return (float, state, Boolean): a tuple of the reward, state observation,
    *         and boolean indicating if it's terminal.
    */
  override def env_step(action: Action): Either[BaseEnvironmentTyped.EnvironmentError, BaseEnvironmentTyped.RewardFeatureState[FantasyParabolicProblem]] = {
    def processAction(): Either[EnvironmentError,RewardFeatureState[FantasyParabolicProblem]] = {

      val indexAction = valid_actions.indexWhere(_ == action)
      if(indexAction >= 0){
        (actions(indexAction),currentState) match {
          case (_,s) if s.totalLength < s.environment.minLengthDivisible =>
            Left(StateIsUnknow(s"State: $currentState is not correctly defined"))
          case (0,s) => Right(RewardFeatureState(s.rewardByCurrentDistribution,s,Finished))
          case (a,_) =>
            currentState.cutAt(a-1).map{
              case (r,Some(np)) =>
                currentState = np
                RewardFeatureState(r,np,OnProcess)
              case (r,None) =>
                environmentFinished = Finished
                RewardFeatureState(r, currentState, Finished)
            }
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
