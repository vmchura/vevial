package gym.evialgame

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointPlanarVector, PointUnitaryVector}

import scala.util.Random
case class EvialGame(parameters: Seq[Point], ropeEje: RopeEje, currentState: Int){
  assert(parameters.nonEmpty)
  def forward(): EvialGame = if(currentState +1 < RopeEje.numElements) copy(currentState = currentState+1) else this
  def backward(): EvialGame = if(currentState > 0) copy(currentState = currentState-1) else this
  def turn(torque: Double): EvialGame = copy(ropeEje = ropeEje.turnRopeNode(currentState,torque))


  lazy val distances = parameters.map{ point =>
    ropeEje.projectPoint(point) match {
      case Some(ep) =>
        Some(ep.toSource match {
          case Some(vec) => vec.magnitude
          case None => 0d
        })
      case None => {
        /*
        println(s"projection of $point is not defined")
        ropeEje.projectPoint(point,true)

         */
        None
      }

    }
  }
  lazy val gameDone: Boolean = !distances.forall(_.isDefined)
  /**
    * less score is better
    */
  lazy val currentScore: Double = {


    if(!gameDone){

      val positionScore = sampleState(parameters.length).map{
        case SampleVector(prog,distance,_) => Math.abs(distance*distance)*Math.sqrt(Math.abs(prog-currentState)) * (if(prog > currentState) 1 else 0)
        case _ => 0d
      }.sum/parameters.length


      distances.flatten.map(d => d*d).sum/parameters.length + Math.sqrt(positionScore)
    }else{
      Double.PositiveInfinity
    }

  }


  def sampleState(): Seq[TSample] = sampleState(EvialGame.SAMPLES_TO_DEFINE_STATE)

  private def sampleState(n: Int): Seq[TSample] = {
    if(gameDone)
      throw new IllegalStateException("Game is done, it should not be sampled")

    val pointsToSample =
      (0 until (n/parameters.length)).flatMap(_ => parameters) ++
        Random.shuffle(parameters).take(n%parameters.length)

    pointsToSample.map{ p =>
      ropeEje.projectPoint(p) match {
        case Some(ep) => {
          val prog = ropeEje.calcProgresiva(ep.ejeElementOwner)
          ep.toSource match {
            case Some(vec) => {
              val z = ep.ejeElementOwner.in.direction x (vec.direction)

              SampleVector(prog,(vec.magnitude*Math.signum(z.z)).toInt,PointPlanarVector(ep.point,vec))
            }
            case None => ExactVector(prog)
          }
        }
        case None => UndefinedSample
      }
    }.sorted
  }
}
object EvialGame{
  val SAMPLES_TO_DEFINE_STATE = 20
}
