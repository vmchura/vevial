package algorithms

import org.scalatest.flatspec.AnyFlatSpec

class DSUTest extends AnyFlatSpec {

  behavior of "DSU intenger"

  it should "create correctly" in {
    val nodes = Array.range(0,10)
    val a = new DSU(nodes)
    def findNumberSets() =  nodes.map(i => a.find_set(i)).distinct.length
    assertResult(10)(findNumberSets())
    a.union_sets(1,2)
    a.union_sets(1,3)
    a.union_sets(1,4)
    a.union_sets(5,6)
    a.union_sets(5,7)
    a.union_sets(5,8)
    a.union_sets(5,9)
    assertResult(3)(findNumberSets())
    a.union_sets(4,0)
    a.union_sets(9,0)
    assertResult(1)(findNumberSets())
  }

}
