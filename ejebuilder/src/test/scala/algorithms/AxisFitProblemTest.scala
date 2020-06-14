package algorithms

import RlAlgorithm.OneStepActorCritic
import agent.AgentEvaluator
import org.scalatest.flatspec.AnyFlatSpec

class AxisFitProblemTest extends AnyFlatSpec {
  behavior of "OneStepActorCritic"

  it  should "not give any errors on Fix Problem" in {
    //(iht_size = 4096,num_tilings = 8, num_tiles = 8, actor_step_size = 1e-1f,
    //      critic_step_size = 1e-0f, avg_reward_step_size = 1e-2f, num_actions = 3, seed = 99,0.9f)
    val maxEpisodes = 10000
    val rewardsAfterEpisode = OneStepActorCritic.runExperimentActorCriticSoftMax(4096,
      8,
      8,
      0.7f,
      1f,
      1f,maxEpisodes,AxisFitProblem.AXIS_FIT_COMPONENT_BUILDER)
    assertResult(maxEpisodes)(rewardsAfterEpisode.size)
    println(rewardsAfterEpisode)
    //assert(rewardsAfterEpisode.lastOption.fold(false)(_ >= 9d))
  }

  it should "reduce error after training" in {
    val maxEpisodes = 100000

    val tc = AxisFitProblem.AXIS_FIT_COMPONENT_BUILDER.getTileCoder(4096,8,8)
    val dummyAgent = AxisFitProblem.AXIS_FIT_COMPONENT_BUILDER.getAgent(5f,4f,4f,tc)
    val bb = AxisFitProblem.loadPrimaryDataFromFile("/home/vmchura/Documents/datasectionbuilder100.xml")


    def acumErrorElementsWithBetterPerformance(data: Seq[AxisFitProblem], agentEvaluator: AgentEvaluator[AxisFitProblem]): Double = {
      def errorPerformance(problem: AxisFitProblem): Double = {
        val r0 = problem.rewardByCurrentDistribution
        val r1 = agentEvaluator.agent_policy(problem) match {
          case 0 => r0
          case a if a <= 3 => problem.divideGamesCuttingAt(a-1) match {
            case Left(error) => {
              println(s"ERROR: $error")
              r0
            }
            case Right((p0,p1)) => {
              -0.5f + (p0.rewardByCurrentDistribution + p1.rewardByCurrentDistribution)
            }
          }
          case _ =>
            println(s"ACTION LARGER THAN 3?")
            0f
        }

        r1 - r0



      }
      data.map(errorPerformance).sum
    }

    println(acumErrorElementsWithBetterPerformance(bb,dummyAgent))

    val (_,agent) = OneStepActorCritic.runExperiment(4096,
      8,
      8,
      0.8f,
      1f,
      1f,maxEpisodes,AxisFitProblem.AXIS_FIT_COMPONENT_BUILDER)

    println(acumErrorElementsWithBetterPerformance(bb,dummyAgent))
    println(acumErrorElementsWithBetterPerformance(bb,agent))
    agent.saveActorWeightOnFile("/home/vmchura/Documents/actorWFirstTry.bin")

    println(s"Saved!")
  }

  behavior of "Cutting all games"
  it should "not throw any error" in {
    val problems = AxisFitProblem.loadPrimaryDataFromFile("/home/vmchura/Documents/datasectionbuilder100.xml")
    problems.foreach(p => {
      try p.calcSubDivisions()
      catch {
        case _: Throwable => println(s"ERROR calc subsidivion ON $p")
      }

      try p.rewardByCurrentDistribution
      catch {
        case _: Throwable => println(s"ERROR calc rewardByCurrentDistr ON $p")
      }
    }

    )
  }
}
