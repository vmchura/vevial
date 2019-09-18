package Layers

import org.scalatest.{Assertion, FlatSpec}
import scalafx.collections.ObservableBuffer

class AggregatedObservableArrayListTest extends FlatSpec {

  def ob2String[T](lista: ObservableBuffer[T]): String = "("+lista.mkString(",")+")"
  "Creation of list of observablelist" should "build with no problems" in {
    val l1 = new ObservableBuffer[Int]()
    l1.addAll(1,2)

    val l2 = new ObservableBuffer[Int]()
    l2.addAll(10,20)

    val aoal = new AggregatedObservableArrayList(Array(l1,l2))
    val listaAgregada = aoal.getAggregatedList()

    assertResult("(1,2,10,20)")(ob2String(listaAgregada))

  }
  behavior of "Removal of one element"

  it should "be reflected deleting not corner cases" in {
    val l1 = new ObservableBuffer[Int]()
    l1.addAll(1,2,3,4)

    val l2 = new ObservableBuffer[Int]()
    l2.addAll(10,20,30,40)

    val aoal = new AggregatedObservableArrayList(Array(l1,l2))
    val listaAgregada = aoal.getAggregatedList()

    l1.remove(0)
    l1.remove(0)
    l2.remove(0)
    l2.remove(1)
    assertResult("(3,4,20,40)")(ob2String(listaAgregada))

  }
  it should "be reflected deleting corner cases" in {
    val l1 = new ObservableBuffer[Int]()
    l1.addAll(1)

    val l2 = new ObservableBuffer[Int]()
    l2.addAll(10)

    val aoal = new AggregatedObservableArrayList(Array(l1,l2))
    val listaAgregada = aoal.getAggregatedList()

    l1.remove(0)
    l2.remove(0)
    assertResult("()")(ob2String(listaAgregada))

  }

  behavior of "Multiple operations"
  val l1 = new ObservableBuffer[Int]()

  val l2 = new ObservableBuffer[Int]()

  val aoal = new AggregatedObservableArrayList(Array(l1,l2))
  val listaAgregada = aoal.getAggregatedList()
  def assertResultLista(expected: String): Assertion = assertResult(expected)(ob2String(listaAgregada))

  it should "adding, removing and update success" in {
    l1.add(5)
    assertResultLista("(5)")
    l2.add(4)
    assertResultLista("(5,4)")
    l1.remove(0)
    assertResultLista("(4)")
    l2.update(0,2)
    assertResult("(2)")(ob2String(l2))
    assertResultLista("(2)")
    l1.add(10)
    assertResultLista("(10,2)")
    l1.add(5)
    l1.update(1,2)
    assertResultLista("(10,2,2)")
    l1.remove(0)
    assertResultLista("(2,2)")
    l1.removeIf(_%2 == 0)
    l2.removeIf(_%2 == 0)
    assertResultLista("()")

  }

}
