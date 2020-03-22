package tiles

class RopeEjeTileCoder(iht_size: Int = 4096, num_tilings: Int = 32, num_tiles: Int = 8) {
  val iht = new IHT(iht_size)

  /**
    * Takes in an angle and angular velocity from the pendulum environment
    * and returns a numpy array of active tiles.
    *
    * @param observation    float, the position
    * @return         tiles -- np.array, active tiles
    */
  def get_tiles(observation: Array[Double]): Array[Int] = {

    //Set the max and min of angle and ang_vel to scale the input (4 lines)
    val SENSOR_MIN = -900d
    val SENSOR_MAX = 900d

    val obsMM = observation.map{ o => Math.min(SENSOR_MIN,Math.max(SENSOR_MAX,o*o))}

    /**
      * ### Use the ranges above and self.num_tiles to set angle_scale and ang_vel_scale (2 lines)
      * # angle_scale = number of tiles / angle range
      * # ang_vel_scale = number of tiles / ang_vel range
      *
      */

    val observation_scale = (num_tiles/(SENSOR_MAX-SENSOR_MIN)).toFloat

    val obs = obsMM.map(_.toFloat*observation_scale)


    Tiles.tileswrap(Right(iht),num_tilings,obs,Seq(num_tiles)).toArray

  }

}
