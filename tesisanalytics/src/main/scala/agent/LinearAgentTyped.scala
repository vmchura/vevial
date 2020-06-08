package agent
import tiles.TileCoderTyped

class LinearAgentTyped(val numActions: Int,
                       val gamma: Float,
                       val actorStepSizeParam: Float,
                       val criticStepSizeParam: Float,
                       val tileCoder: TileCoderTyped[Int]) extends BaseAgentTyped[Int] {
  override def defaultObs: Int = -1
}


object LinearAgentTyped {

  def buildTileCoder(ihtSizeParam: Int, numTilingsParam: Int, numTilesParam: Int): TileCoderTyped[Int] = {
    new TileCoderTyped[Int] {
      override val ihtSize: Int = ihtSizeParam

      override val numTilings: Int = numTilingsParam

      override val numTiles: Int = numTilesParam

      override def buildObs(input: Int): Seq[Float] = {
        val POSITION_MIN = 0d
        val POSITION_MAX = 10d
        val position_scaled: Float = (input/(POSITION_MAX-POSITION_MIN)).toFloat
        Seq(position_scaled)
      }
    }
  }

}
