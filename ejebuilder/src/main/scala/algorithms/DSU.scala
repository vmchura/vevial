package algorithms

import scala.collection.mutable

class DSU[A](nodes: Seq[A]) {
  val parent: mutable.Map[A, A] = mutable.Map(nodes.map(a => a -> a):  _*)
  val size: mutable.Map[A,Int] = mutable.Map(nodes.map(a => a -> 1):  _*)

  def find_set(a: A): A = {
    if(parent(a) == a)
      a
    else{
      parent(a) = find_set(parent(a))
      parent(a)
    }
  }
  def union_sets(a: A, b: A): Unit = {
    val ap = find_set(a)
    val bp = find_set(b)
    if(ap!=bp){
      val (app,bpp) = if(size(ap) < size(bp)) (bp,ap) else (ap,bp)
      parent(bpp) = app
      size(app) += size(bpp)
    }
  }

  override def toString: String = {
    nodes.map(a => s"[$a,${find_set(a)}]").mkString(" ")
  }
}
