package models

import org.scalatest.flatspec.AnyFlatSpec

class TreeGraphTest extends AnyFlatSpec {

  behavior of "TreeGraph into linear"
  it should "not create invalid linear graphs" in {
    val t0 = TreeGraph(Nil,Set.empty[Edge[IntNode]])
    assertResult(LinearGraph[IntNode](List()))(t0.toLinearGraph())

    val nodes: Array[IntNode] = (0 until 3).map(IntNode).toArray
    val edges1 = Set(
      Edge(nodes(0),nodes(1)),
      Edge(nodes(1),nodes(0)),
      Edge(nodes(1),nodes(2)),
      Edge(nodes(2),nodes(1)),
      Edge(nodes(2),nodes(0)),
      Edge(nodes(0),nodes(2))
    )

    val t1 = TreeGraph(nodes,edges1)
    val x0 = (t1.toLinearGraph()) == (LinearGraph(List(nodes(1), nodes(0), nodes(2))))
    val x1 = (t1.toLinearGraph()) == (LinearGraph(List(nodes(1), nodes(2), nodes(0))))
    val x2 = (t1.toLinearGraph()) == (LinearGraph(List(nodes(2), nodes(0), nodes(1))))
    val x3 = (t1.toLinearGraph()) == (LinearGraph(List(nodes(2), nodes(1), nodes(0))))
    val x4 = (t1.toLinearGraph()) == (LinearGraph(List(nodes(0), nodes(1), nodes(2))))
    val x5 = (t1.toLinearGraph()) == (LinearGraph(List(nodes(0), nodes(2), nodes(1))))

    assert(x0 || x1 || x2 || x3 || x4 || x5)

    val edges2 = Set(
      Edge(nodes(0),nodes(1)),
      Edge(nodes(1),nodes(0)),
      Edge(nodes(0),nodes(2)),
      Edge(nodes(2),nodes(0))
    )
    val t2 = TreeGraph(nodes,edges2)
    val r0 = (t2.toLinearGraph()) == (LinearGraph(List(nodes(1), nodes(0), nodes(2))))
    val r1 = (t2.toLinearGraph()) == (LinearGraph(List(nodes(2), nodes(0), nodes(1))))
    assert(r0 || r1)
  }

}
