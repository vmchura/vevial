package gym

import environment.BaseEnvironmentTyped.{EnvironmentError, InvalidAction, Reward}

class FantasyParabolicProblem(val data: List[FantasyParabolicData],val minData: Double, val maxData: Double) extends ProblemDivisible[FantasyParabolicProblem,FantasyParabolicData] {
  val environment: ProblemDivisbleEnvironment[FantasyParabolicProblem, FantasyParabolicData] = FantasyParabolicEnvironment



  override def cutAt(nCut: Int): Either[EnvironmentError,(Reward,Option[FantasyParabolicProblem])] = {

    val cut = subDivisionsCutAt(nCut)


    val (d0,d1) = data.map(d => d.copy(y = d.y*4f/5f)).partition(_.toCompare < cut)
    val p0 = new FantasyParabolicProblem(d0,minData,cut)
    val p1 = new FantasyParabolicProblem(d1,cut,maxData)
    val newReward: Reward = {
      val rewardByCuttingIt = environment.rewardByCut + List(p0,p1).map(_.rewardByCurrentDistribution).sum
      val increaseReward = rewardByCuttingIt - rewardByCurrentDistribution
      if(increaseReward > 0){
        Math.min(increaseReward,5f)
      }
      else
        -1f
    }

    val newProblem = nCut match {
      case 0 => if(p1.totalLength > environment.minLengthDivisible) Some(p1)  else  None
      case 1 => None
      case 2 => if(p0.totalLength > environment.minLengthDivisible) Some(p0)  else  None
      case _ => None
    }




    if(nCut > 2)
      Left(InvalidAction(s"cut at $nCut is not defined"))
    else
      Right((newReward,newProblem))

  }


  override def rewardByCurrentDistribution: Reward = {
      - chunksDontPassLimitCriteria()*1f
  }

  override def calcSubDivisions(): List[FantasyParabolicProblem] = {
    val divisions: List[Double] = minData :: (subDivisionsCutAt.toList ::: (maxData :: Nil))
    divisions.zip(divisions.tail).map{ case (a,b) =>
      new FantasyParabolicProblem(data.filter(d => a <= d.toCompare && d.toCompare < b),a,b)
    }

  }

  override def chunksDontPassLimitCriteria(): Int = Math.min(calcChunks().count(environment.isChunkBadFormed),environment.limitChunksFailCriteria)

  override def toString: String = s"FantasyParabolicProblem[Data: $data, $minData - $maxData]"
}


