package algorithms


import environment.BaseEnvironmentTyped
import environment.BaseEnvironmentTyped.{Action, EnvironmentError, InvalidAction, RewardFeatureState, StateIsTerminal, StateIsUnknow, TerminalState}
import environment.BaseEnvironmentTyped.TerminalState.{Finished, OnProcess, UnknowState}

import scala.util.Random

class AxisFitEnvironmentActorCritic  extends BaseEnvironmentTyped[AxisFitProblem]{
  private val valid_actions:  Array[Action] = Array.range(0,4)
  private val actions: Array[Int] = Array.range(0,4)

  private val dataFromFile = AxisFitProblem.loadPrimaryDataFromFile("/home/vmchura/Documents/datasectionbuilder100.xml").toArray
  private val numDataElementsFromFile = dataFromFile.length

  private var currentState: AxisFitProblem = dataFromFile(Random.nextInt(numDataElementsFromFile))

  private var environmentFinished: TerminalState.Value = UnknowState

  private var initialReward = 0f
  private var numberCuts = 0
  private var finalReward = 0f
  /**
    * The first method called when the experiment starts, called before the
    * agent starts.
    *
    * @return The first state observation from the environment.
    */
  override def env_start(): AxisFitProblem = {
    currentState = dataFromFile(Random.nextInt(numDataElementsFromFile))
    initialReward = currentState.rewardByCurrentDistribution
    numberCuts = 0
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
  override def env_step(action: Action): Either[BaseEnvironmentTyped.EnvironmentError, BaseEnvironmentTyped.RewardFeatureState[AxisFitProblem]] = {
    def processAction(): Either[EnvironmentError,RewardFeatureState[AxisFitProblem]] = {

      val indexAction = valid_actions.indexWhere(_ == action)
      if(indexAction >= 0){
        println(s"action taken: ${actions(indexAction)}")
        val resAfterAction = (actions(indexAction),currentState) match {
          case (_,s) if s.totalLength < s.environment.minLengthDivisible =>
            Left(StateIsUnknow(s"State: $currentState is not correctly defined"))
          case (0,s) =>

            Right(RewardFeatureState(s.rewardByCurrentDistribution,s,Finished))
          case (a,_) =>
            numberCuts += 1
            currentState.cutAt(a-1).map{
              case (r,Some(np)) =>
                currentState = np
                RewardFeatureState(r,np,OnProcess)
              case (r,None) =>
                environmentFinished = Finished
                RewardFeatureState(r, currentState, Finished)
            }

        }

        finalReward = resAfterAction match {
          case Right(x) => x.reward
          case Left(_) => 0f
        }
        resAfterAction match {
          case Left(e) => println(f"ERROR $e $initialReward%5.1f => $numberCuts%4d => $finalReward%5.1f")
          case Right(RewardFeatureState(_,_,Finished)) => ()//println(f"Finished $initialReward%5.1f => $numberCuts%4d => $finalReward%5.1f")
          case _ => ()
        }

        resAfterAction

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
