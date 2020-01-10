package gym.evialgame

import org.scalatest.FlatSpec

class RopeEjeTest extends FlatSpec {

  behavior of "turn right"
  it should "print expected values" in {
    val ropeEje = RopeEje()
    println(ropeEje)
    val turned = ropeEje.turnRopeNode(0,10)
    println(turned)
  }

}
