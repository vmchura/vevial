package tiles

import org.scalatest.flatspec.AnyFlatSpec

class PendulumTileCoderTest extends AnyFlatSpec {

  behavior of "Pendulum tile"
  it should "give the correct expected value" in {
    val pdtc = new PendulumTileCoder(iht_size = 4096,num_tilings = 8, num_tiles = 4)



    val test_obs: Array[Seq[Float]] = Array(Seq(-Math.PI.toFloat,0f),
      Seq(-Math.PI.toFloat,0.5f),
      Seq(Math.PI.toFloat,0f),
      Seq(Math.PI.toFloat,-0.5f),Seq(0f,1f))



    def get_tiles(obs: Seq[Float]): Array[Int] = {
      val Seq(angle,ang_vel) = obs
      pdtc.get_tiles(angle= angle,ang_vel = ang_vel)
    }
    assertResult(Array(0,1,2,3,4,5,6,7))(get_tiles(test_obs(0)))
    assertResult(Array(0,1,2,3,4,8,6,7))(get_tiles(test_obs(1)))
    assertResult(Array(0,1,2,3,4,5,6,7))(get_tiles(test_obs(2)))
    assertResult(Array(9,1,2,10,4,5,6,7))(get_tiles(test_obs(3)))
    assertResult(Array(11,12,13,14,15,16,17,18))(get_tiles(test_obs(4)))

  }
}
