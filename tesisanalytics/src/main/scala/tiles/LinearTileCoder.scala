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
class LinearTileCoder(iht_size: Int = 4096, num_tilings: Int = 32, num_tiles: Int = 8) {
  val iht = new IHT(iht_size)

  /**
    * Takes in an angle and angular velocity from the pendulum environment
    * and returns a numpy array of active tiles.
    *
    * @param position    float, the position
    * @return         tiles -- np.array, active tiles
    */
  def get_tiles(position: Float): Array[Int] = {

    //Set the max and min of angle and ang_vel to scale the input (4 lines)
    val POSITION_MIN = 0d
    val POSITION_MAX = 10d

    /**
      * ### Use the ranges above and self.num_tiles to set angle_scale and ang_vel_scale (2 lines)
      * # angle_scale = number of tiles / angle range
      * # ang_vel_scale = number of tiles / ang_vel range
      *
      */

    val position_scala: Float = (num_tiles/(POSITION_MAX-POSITION_MIN)).toFloat

    val obs = Seq(position*position_scala)


    Tiles.tileswrap(Right(iht),num_tilings,obs,Seq(num_tiles)).toArray

  }

}
