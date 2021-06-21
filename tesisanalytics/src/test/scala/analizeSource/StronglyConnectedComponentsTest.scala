package analizeSource

import org.scalatest.flatspec.AnyFlatSpec

class StronglyConnectedComponentsTest extends AnyFlatSpec {
  behavior of "Strongly Connected Componentes"
  it should "Build correctly" in {
    val nodes = (0 to 4).map(i => i -> i.toString).toMap
    val edges = Map(0 -> List(2, 3), 1 -> List(0), 2 -> List(1), 3 -> List(4))
    val scc = StronglyConnectedComponents.buildSCC(nodes, edges)
    println(scc.map(_.mkString(",")).map("[" + _ + "]").mkString("/"))
  }
}
