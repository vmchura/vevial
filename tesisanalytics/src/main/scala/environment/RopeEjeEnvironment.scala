package environment

import gym.evialgame.{ExactVector, RopeEje, SampleVector, SampleWithProg, TSample}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointPlanarVector}

import scala.util.Random

class RopeEjeEnvironment() extends BaseEnvironment {

  val nodesToTry = 10


  var valid_actions:  Array[Int] = null
  var actions: Array[(Int,Int)] = null
  var last_action = -1

  var ropeEje: RopeEje = null
  val pointsSource: Seq[Point] = (1 to 100).flatMap{ x =>
    val y = x*x/1000d
    List(Point(x,y+2.5),Point(x,y-2.5))
  }
  val obsPoints: Int = 10
  def getObs(): Array[Double] = {
    val t: Seq[SampleWithProg] = pointsSource.flatMap{ p =>
      ropeEje.projectPoint(p).map{ ep =>
        val prog = ropeEje.calcProgresiva(ep.ejeElementOwner)
        ep.toSource match {
          case Some(vec) => {
            val z = ep.ejeElementOwner.in.direction x (vec.direction)

            SampleVector(prog,(vec.magnitude*Math.signum(z.z)).toInt,PointPlanarVector(ep.point,vec))
          }
          case None => ExactVector(prog)
        }

      }
    }
    if(t.length < pointsSource.length)
      Array.fill(obsPoints)(100d)
    else{
      val obs = Array.fill(obsPoints)(0d)
      val count = Array.fill(obsPoints)(0)
      t.foreach{ s =>
        val indx = Math.min((s.prog/10),obsPoints-1)
        val d = s match {
          case SampleVector(_,distance,_) => distance
          case _ => 0
        }
        obs(indx) += d
        count(indx) += 1
      }
      (obs,count).zipped.map{
        case (o,c) => if(c>0) o*1d/c else 0d
      }.toArray
    }
  }
  private def getGlobalError(): Double = pointsSource.flatMap{ p =>
    ropeEje.projectPoint(p).flatMap(_.toSource.map(m => m.magnitude*m.magnitude))
  }.sum



  var maxError: Double = 0
  override var reward_obs_term: (Float,Array[Double], Boolean) = null


  /**
    * Setup for the environment called when the experiment first starts
    *
    * @param env_info
    */
  override def env_init(env_info: EnvInfo): Unit = {
    Random.setSeed(env_info.seed)
    ropeEje = RopeEje()
    maxError = getGlobalError()*2


    valid_actions = Array.range(0,nodesToTry*2+1)
    actions = Array((0,0))++Array.range(0,nodesToTry).flatMap(i => Array((i*10,-10),(i*10,10)))
    last_action = -1

  }

  /**
    * The first method called when the experiment starts, called before the
    * agent starts.
    *
    * @return The first state observation from the environment.
    */
override def env_start(): Array[Double] = {

  val reward = 0f
  val observation = getObs()
  val is_terminal = false

  reward_obs_term = (reward,observation,is_terminal)
  reward_obs_term._2
}

  /**
    * A step taken by the environment.
    *
    * @param action The action taken by the agent
    * @return (float, state, Boolean): a tuple of the reward, state observation,
    *         and boolean indicating if it's terminal.
    */
override def env_step(action: Int): (Float,Array[Double], Boolean) = {
  assert(valid_actions.contains(action))
  val (indx,turn) = actions(action)

  ropeEje = ropeEje.turnRopeNode(indx,turn)
  val error = getGlobalError()
  if(error > maxError){
    ropeEje = RopeEje()
  }


  val obs = getObs()
  val reward = -obs.map(x => x*x).sum
  last_action = action


  //val reward = -(last_state.map(x => x * x).sum)
  val is_terminal = false
  reward_obs_term = (reward.toFloat,obs,is_terminal)
  reward_obs_term

}

  /**
    * Cleanup done after the environment ends
    */
override def env_cleanup(): Unit = ()

  /**
    * A message asking the environment for information
    *
    * @param message
    * @return
    */
override def env_message(message: String): Double = -1d
}
