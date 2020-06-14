package algorithms

import java.io.File

import RlAlgorithm.ExperimentBuilderComponents
import agent.{AgentBuilderTyped, AgentEvaluator, BaseAgentTyped}
import breeze.linalg.DenseMatrix
import environment.BaseEnvironmentTyped
import environment.BaseEnvironmentTyped.{Action, Reward}
import gym.{ProblemDivisbleEnvironment, ProblemDivisible}
import io.vmchura.vevial.PlanarGeometric.BasicEje.{EfficientSeqEjeElements, TSeqEjeElementsBase}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TDirection}
import io.vmchura.vevial.elementdata.{UDirection, UPoint}
import tiles.TileCoderTyped

import scala.xml.{Elem, XML}

case class AxisFitProblem private (in: PointUnitaryVector, out: PointUnitaryVector, dataToMatch: Seq[ProgPointTangent], data: List[DataWithProjectionError], minData: Double, maxData: Double) extends BuilderFixedPoints with ProblemDivisible[AxisFitProblem,DataWithProjectionError] {

  override def toString: String = f"AF[$minData%4.2f - $maxData%4.2f]"

  val environment: ProblemDivisbleEnvironment[AxisFitProblem, DataWithProjectionError] = AxisFitEnvironment

  private def findAveragePoint(prog: Double): PointUnitaryVector = {
    type PVMeta = (UPoint,UDirection)

    val pointsClose: Seq[PVMeta] = dataToMatch.map(pp => (Math.abs(pp.prog - prog), pp)).filter(f => f._1 < 20 && f._2.pointTangent.point.isDefined).sortBy(_._1).flatMap{case (_,f) => f.pointTangent.point.map(p => (p,f.pointTangent.tangent))}
    val u = pointsClose.reduceLeftOption[PVMeta]{case ((a,b),(x,y)) => (a |-| x, b |-| y)}
    //FIXME even though there are no items, it can be found a point unitary vector based on in-out points
    u.fold(throw new IllegalArgumentException("cant cut here! there are no items")){ case (pd,dd) =>  PointUnitaryVector(pd.value,dd.value)}
  }
  override protected def buildElements: Either[Exception, TSeqEjeElementsBase] = {
    def toBasicSectionBuilder(bb: AxisFitProblem): BasicSectionBuilder = BasicSectionBuilder(bb.in,bb.out,Nil)
    def continueBuilding(currentState: AxisFitProblem): Either[Exception,List[BasicSectionBuilder]] = {
      val defaultValue = Right(List(toBasicSectionBuilder(currentState)))
      AxisFitProblem.reinforcementLearningPolicy(currentState) match {
        case 0 => defaultValue
        case a if a <= 3 =>
          (divideGamesCuttingAt(a-1),cutAt(a-1)) match {
            case (Right((left,right)),Right((_,Some(taken)))) =>
              a-1 match {
                case 0 => continueBuilding(taken).map(lista => lista ::: toBasicSectionBuilder(right) :: Nil)
                case 2 => continueBuilding(taken).map(lista => toBasicSectionBuilder(left) :: lista)
                case _ => Left(new IllegalStateException(s"IT SHOULD NOT ARRIVE HERE: cut at ${a-1} and the game continues?"))
              }
            case (Right((left,right)),Right((_,None))) =>
              Right(toBasicSectionBuilder(left) :: toBasicSectionBuilder(right) :: Nil)
            case res =>{
              println(s"res got: $res")
              defaultValue
            }
          }

        case _ => defaultValue
      }
    }

    continueBuilding(this).flatMap(lista => {
      val elements = lista.map(_.elements)
      val errors = for(Left(i) <- elements) yield i
      val elementsEje = for(Right(i) <- elements) yield i
      if(errors.nonEmpty)
        Left(errors.head)
      else
        elementsEje.reduceLeftOption(_ append _).fold(Left(new IllegalStateException("there are no items to build")): Either[Exception,TSeqEjeElementsBase])(x => Right(x))
    })
  }

  override def calcSubDivisions(): List[AxisFitProblem] = {
    val progPoints: List[PointUnitaryVector] = in :: subDivisionsCutAt.toList.map(findAveragePoint) ::: (out :: Nil)
    val divisions: List[Double] = minData :: (subDivisionsCutAt.toList ::: (maxData :: Nil))
    val subDivisions = divisions.zip(divisions.tail).zip(progPoints.zip(progPoints.tail)).map{
      case ((min,max),(nin,nout)) =>
        val ndataToMatch = dataToMatch.filter(d => min <= d.prog && d.prog < max)
        AxisFitProblem(nin,nout,ndataToMatch,min,max)
    }

    val errors = for(Left(i) <- subDivisions) yield i
    if(errors.nonEmpty){
      throw new IllegalStateException(s"ERRORS EXISTS: ${errors.mkString("\n")}")
    }
    val subdivision = for(Right(i) <- subDivisions) yield i

    subdivision

  }

  def divideGamesCuttingAt(nCut: Int): Either[Exception, (AxisFitProblem,AxisFitProblem)] = {

    val cut = subDivisionsCutAt(nCut)
    val progPoint = try Some(findAveragePoint(cut)) catch {
      case _: Throwable => None
    }
    val (d0, d1) = dataToMatch.partition(_.prog < cut)
    progPoint match {
      case Some(middle) =>
        for {
          p0 <- AxisFitProblem(in, middle, d0, minData, cut)
          p1 <- AxisFitProblem(middle, out, d1, cut, maxData)
        } yield {
          (p0, p1)
        }
      case None => Left(new IllegalStateException("Cant Cut here"))
    }

  }

  override def cutAt(nCut: Int): Either[BaseEnvironmentTyped.EnvironmentError, (Reward, Option[AxisFitProblem])] = {

    divideGamesCuttingAt(nCut) match {
      case Right((p0,p1)) =>
        val newReward: (Reward, Option[AxisFitProblem]) = {

            val rewardByCuttingIt = environment.rewardByCut + List(p0, p1).map(_.rewardByCurrentDistribution).sum
            val increaseReward = rewardByCuttingIt - rewardByCurrentDistribution
            val nr = if (increaseReward > 0) {
              Math.min(increaseReward, 5f)
            }
            else
              -1f

            val np = nCut match {
              case 0 => if (p1.totalLength > environment.minLengthDivisible) Some(p1) else None
              case 1 => None
              case 2 => if (p0.totalLength > environment.minLengthDivisible) Some(p0) else None
              case _ => None
            }
            (nr, np)

        }
        Right(newReward)

      case Left(error) =>
        println(s"ERROR in cutAt: $error")
        Right((0f,None))

    }
  }

  override def rewardByCurrentDistribution: Reward =   - chunksDontPassLimitCriteria()*1f

}
object AxisFitProblem{
  def apply(in: PointUnitaryVector, out: PointUnitaryVector, dataToMatch: Seq[ProgPointTangent], minData: Double, maxData: Double): Either[Exception,AxisFitProblem] = {

       BasicSectionBuilder(in,out,Nil).elements.map{ ejeBasic =>
         val eje = EfficientSeqEjeElements(ejeBasic)
        val dataPT_DD = dataToMatch.flatMap(pt => pt.pointTangent.point.flatMap(p => eje.projectPoint(p.value).map(ep => (pt,new DataWithProjectionError(pt.prog,ep.lengthToPointAbs.toFloat)))))
        val progPointTangentPoints = dataPT_DD.map(_._1)
        val dataDivisible = dataPT_DD.map(_._2).toList
        new AxisFitProblem(in,out,progPointTangentPoints,dataDivisible,minData,maxData)
      }
  }
  val DummyAxisFitProblem: AxisFitProblem = AxisFitProblem(PointUnitaryVector(Point(0,0),TDirection(1,0)),PointUnitaryVector(Point(-10d,-10d),TDirection(0,-1)),Nil,Nil,0d,1d)

  def reinforcementLearningPolicy(problem: AxisFitProblem): Action = {
    val actionTaken = AxisFitProblem.agentTrained.agent_policy(problem)
    println(s"action taken: $actionTaken")
    actionTaken
  }

  def loadPrimaryDataFromFile(path: String): Seq[AxisFitProblem] = {
    val file = new File(path)
    val doc: Elem = XML.loadFile(file)
    val validAxisFitProblem = for(Right(i) <- (doc \\ "SectionBuilder").flatMap(BasicSectionBuilder.loadBasicSectionBuilderFromNodeXML).map(bb => {
      val data: Seq[Double] = bb.dataToMatch.map(_.prog.toDouble)
      AxisFitProblem(bb.in, bb.out, bb.dataToMatch, data.minOption.getOrElse(0d),data.maxOption.getOrElse(0d))
    })) yield i

    println(s"LOADED BASIC SECTION BUILDER: ${validAxisFitProblem.length}")
    validAxisFitProblem
  }


  val AXIS_FIT_COMPONENT_BUILDER: ExperimentBuilderComponents[AxisFitProblem] = new ExperimentBuilderComponents[AxisFitProblem] {

    override def getTileCoder(ihtSize: Int, numTilings: Int, numTiles: Int): TileCoderTyped[AxisFitProblem] = AgentBuilderTyped.buildTileCoder[AxisFitProblem,DataWithProjectionError](ihtSize,numTilings,numTiles)

    override def getAgent(gamma: Float, actorStepSize: Float, criticStepSize: Float, tileCoder: TileCoderTyped[AxisFitProblem]): BaseAgentTyped[AxisFitProblem] = new AgentBuilderTyped[AxisFitProblem,DataWithProjectionError](4,gamma,actorStepSize,criticStepSize,tileCoder,DummyAxisFitProblem)

    override def getEnvironment: BaseEnvironmentTyped[AxisFitProblem] = new AxisFitEnvironmentActorCritic()
  }

  val agentTrained: AgentEvaluator[AxisFitProblem] = new AgentEvaluator[AxisFitProblem] {

    override def tileCoder: TileCoderTyped[AxisFitProblem] = AXIS_FIT_COMPONENT_BUILDER.getTileCoder(4096,8,8)

    override def actorW: DenseMatrix[Double] = breeze.linalg.csvread(new File("/home/vmchura/Documents/actorWFirstTry.bin"))

  }




}
