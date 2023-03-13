package Layers

import scalafx.scene.Node

import scala.collection.mutable

trait TLayer[E] extends ObservableNodes{

  def conversor(e: E): Seq[Node]
  private val map2Nodes: mutable.Map[E,Seq[Node]] = mutable.Map[E,Seq[Node]]()
  def add(e: E): Unit = {
    val s = conversor(e)
    map2Nodes += e -> s
    nodes.appendAll(s)
  }
  def addAll(listE: Iterable[E]): Unit = {
    val ss = listE.map(e => e->conversor(e)).toMap
    map2Nodes ++= ss
    nodes.appendAll(ss.values.flatten)
  }
  def remove(e: E): Unit = {
    map2Nodes.get(e).map{ s =>
      nodes.removeIf(n => s.contains(n))
    }
    map2Nodes.remove(e)
  }
  def removeAll(listE: Iterable[E]): Unit = {
    val s: Seq[Node] = listE.flatMap(e => map2Nodes.getOrElse(e,Nil)).toList
    nodes.removeIf(n => s.contains(n))
    listE.foreach(map2Nodes.remove)
  }
  protected def elementsDrawn(): scala.collection.Set[E] = map2Nodes.keySet

  def clear(): Unit = {
    removeAll(elementsDrawn())
  }
  def update(): Unit

}
