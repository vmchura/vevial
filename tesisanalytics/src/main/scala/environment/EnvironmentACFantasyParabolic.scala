package environment

import environment.BaseEnvironmentTyped.{Action, EnvironmentError, InvalidAction, RewardFeatureState, StateIsTerminal, StateIsUnknow, TerminalState}
import environment.BaseEnvironmentTyped.TerminalState.{Finished, OnProcess, UnknowState}
import gym.{FantasyParabolicData, FantasyParabolicProblem}

import scala.util.Random

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
    val length = Random.nextFloat()*1000f+100
    val points = Random.nextInt(100)
    val errorAt = Random.nextFloat()*length
    val data = List.fill(points){
      val pointAt = Random.nextFloat()*length
      val distance = Math.abs(errorAt-pointAt)
      val scale: Float = Math.min(6f,50f/distance)
      FantasyParabolicData(pointAt,Random.nextFloat()*scale)
    }

    currentState = new FantasyParabolicProblem(data,0,length)
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
          case (0,s) => Right(RewardFeatureState(0f,s,Finished))
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
