package gym

import environment.BaseEnvironmentTyped.{EnvironmentError, InvalidAction, Reward}

class FantasyParabolicProblem(val data: List[FantasyParabolicData]) extends ProblemDivisible[FantasyParabolicProblem,FantasyParabolicData] {
  val environment: ProblemDivisbleEnvironment[FantasyParabolicProblem, FantasyParabolicData] = FantasyParabolicEnvironment

  override val subDivisions: Array[Double] = {
    val delta = (maxData.toCompare - minData.toCompare)/(environment.numCuts+1)
    Array.range(1,environment.numCuts+1).map{ i =>  i*delta + minData.toCompare}
  }


  override def cutAt(nCut: Int): Either[EnvironmentError,(Reward,Option[FantasyParabolicProblem])] = {
    val cut = subDivisions(nCut)
    val (d0,d1) = data.map(d => d.copy(y = d.y*4f/5f)).partition(_.toCompare < cut)
    val p0 = new FantasyParabolicProblem(d0)
    val p1 = new FantasyParabolicProblem(d1)
    val newReward: Reward = environment.rewardByCut + (nCut match {
      case 0 => p0.rewardByCurrentDistribution
      case 1 => List(p0,p1).map(_.rewardByCurrentDistribution).sum
      case 2 => p1.rewardByCurrentDistribution
      case _ => -1f
    })

    val newProblem = nCut match {
      case 0 => Some(p1)
      case 1 => None
      case 2 => Some(p0)
      case _ => None
    }
    if(nCut > 2)
      Left(InvalidAction(s"cut at $nCut is not defined"))
    else
      Right((newReward,newProblem))

  }

  override def calcChunks(): List[List[FantasyParabolicData]] = {
    val L = environment.lengthChunks
    val delta = maxData.toCompare - minData.toCompare
    val k = (delta/L).toInt + 1

    (0 until k).map(i => {
      val ini = i*L + minData.toCompare
      val end = if(i == k-1) maxData.toCompare else ini + L
      data.filter(d => ini <= d.toCompare && d.toCompare < end)
    }).toList

  }

  override def rewardByCurrentDistribution: Reward = {
    if(totalLength < environment.minLengthDivisible)
      -10f
    else {
      val badChunks = Math.max(calcChunks().count(environment.isChunkBadFormed),environment.limitChunksFailCriteria)
      - badChunks*1f

    }
  }

}


