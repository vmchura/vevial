package tiles

/**
  * Initializes the MountainCar Tile Coder
  * Initializers:
  * iht_size -- int, the size of the index hash table, typically a power of 2
  * num_tilings -- int, the number of tilings
  * num_tiles -- int, the number of tiles. Here both the width and height of the tiles are the same
  *
  * @param iht_size   tc.IHT, the index hash table that the tile coder will use
  * @param num_tilings int, the number of tilings the tile coder will use
  * @param num_tiles  int, the number of tiles the tile coder will use
  */
class PendulumTileCoder(iht_size: Int = 4096, num_tilings: Int = 32, num_tiles: Int = 8) {
  val iht = new IHT(iht_size)

  /**
    * Takes in an angle and angular velocity from the pendulum environment
    * and returns a numpy array of active tiles.
    *
    * @param angle    float, the angle of the pendulum between -np.pi and np.pi
    * @param ang_vel  float, the angular velocity of the agent between -2*np.pi and 2*np.pi
    * @return         tiles -- np.array, active tiles
    */
  def get_tiles(angle: Float, ang_vel: Float): Array[Int] = {

    //Set the max and min of angle and ang_vel to scale the input (4 lines)
    val ANGLE_MIN = -Math.PI
    val ANGLE_MAX = Math.PI
    val ANG_VEL_MIN = -2*Math.PI
    val ANG_VEL_MAX = 2*Math.PI

    /**
      * ### Use the ranges above and self.num_tiles to set angle_scale and ang_vel_scale (2 lines)
      * # angle_scale = number of tiles / angle range
      * # ang_vel_scale = number of tiles / ang_vel range
      *
      */

    val angle_scale: Float = (num_tiles/(ANGLE_MAX-ANGLE_MIN)).toFloat
    val ang_vel_scale: Float = (num_tiles/(ANG_VEL_MAX-ANG_VEL_MIN)).toFloat

    val obs = Seq(angle*angle_scale,ang_vel*ang_vel_scale)


    Tiles.tileswrap(Right(iht),num_tilings,obs,Seq(num_tiles)).toArray

  }

}
