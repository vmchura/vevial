package tiles

trait TileCoderTyped[I] {
  def ihtSize: Int
  def numTilings: Int
  def numTiles: Int
  lazy val iht = new IHT(ihtSize)
  final def getTiles(input: I): Array[Int] = {
    val obs = buildObs(input)
    Tiles.tileswrap(Right(iht),numTilings,obs,Seq(numTiles)).toArray
  }
  def buildObs(input: I): Seq[Float]
}
