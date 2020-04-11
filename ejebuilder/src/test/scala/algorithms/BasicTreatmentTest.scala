package algorithms

import models.{Edge, Graph, IntNode, TNode}
import org.scalatest.flatspec.AnyFlatSpec

class BasicTreatmentTest extends AnyFlatSpec {

  behavior of "Run basic graph"
  it should "give correct results on int graph" in {


    val nodes: Array[IntNode] =  (0 to 5).map(IntNode).toArray
    val edges: Set[Edge[IntNode]] = Set(
      Edge(nodes(0),nodes(1)),
      Edge(nodes(1),nodes(2)),
      Edge(nodes(2),nodes(3)),
      Edge(nodes(3),nodes(4)),
      Edge(nodes(4),nodes(5))

    )

    val r = BasicTreatment.buildTree(Graph(nodes,edges))
    assertResult(1)(r.length)
    assertResult(6)(r.head.nodes.length)
  }

  it should "give correct results on cycle int graph" in {
    case class IntNode(v: Int) extends TNode[IntNode]{
      override def toString: String = v.toString
    }

    val nodes: Array[IntNode] =  (0 to 5).map(IntNode).toArray
    val edges: Set[Edge[IntNode]] = Set(
      Edge(nodes(0),nodes(1)),
      Edge(nodes(1),nodes(2)),
      Edge(nodes(2),nodes(0)),
      Edge(nodes(2),nodes(3)),
      Edge(nodes(3),nodes(4)),
      Edge(nodes(4),nodes(5))

    )

    val r = BasicTreatment.buildTree(Graph(nodes,edges))
    assertResult(1)(r.length)
    assertResult(4)(r.head.nodes.length)
  }
  it should "give correct results on 2  cycle int graph" in {
    case class IntNode(v: Int) extends TNode[IntNode]{
      override def toString: String = v.toString
    }

    val nodes: Array[IntNode] =  (0 to 6).map(IntNode).toArray
    val edges: Set[Edge[IntNode]] = Set(
      Edge(nodes(0),nodes(1)),
      Edge(nodes(1),nodes(2)),
      Edge(nodes(2),nodes(3)),
      Edge(nodes(3),nodes(1)),
      Edge(nodes(1),nodes(4)),
      Edge(nodes(4),nodes(5)),
      Edge(nodes(5),nodes(1)),
      Edge(nodes(2),nodes(6)),

    )

    val r = BasicTreatment.buildTree(Graph(nodes,edges))
    assertResult(1)(r.length)
    assertResult(3)(r.head.nodes.length)

  }
}
