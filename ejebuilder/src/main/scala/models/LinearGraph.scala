package models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint

import scala.collection.mutable.ListBuffer

case class LinearGraph[A <: TNode[A]](nodes: Seq[A]) extends TGraph[A] {
  override def edges: Set[Edge[A]] = throw new NotImplementedError()
  def reverse(): LinearGraph[A] = LinearGraph(nodes.reverse)
  def ++(other: LinearGraph[A]): LinearGraph[A] =
    LinearGraph(nodes ++ other.nodes)
}

object LinearGraph {
  def mergeLinearGraphs(
      lines: Seq[LinearGraph[GeoNode]]
  ): LinearGraph[GeoNode] = {
    require(lines.nonEmpty, "mergeLinearGraphs: lines are empty")
    def closest(
        getterTarget: LinearGraph[GeoNode] => TPoint
    )(getterTest: LinearGraph[GeoNode] => TPoint)(
        x: LinearGraph[GeoNode],
        a: Seq[LinearGraph[GeoNode]]
    ): (LinearGraph[GeoNode], Int) = {
      val res = a.minBy(y => !(getterTest(y) - getterTarget(x)))
      (res, (!(getterTest(res) - getterTarget(x))).toInt)
    }

    val ini: LinearGraph[GeoNode] => TPoint = _.nodes.head.center
    val end: LinearGraph[GeoNode] => TPoint = _.nodes.last.center

    val closestFromIniToIni = closest(ini)(ini) _
    val closestFromIniToEnd = closest(ini)(end) _
    val closestFromEndToIni = closest(end)(ini) _
    val closestFromEndToEnd = closest(end)(end) _

    val lb = ListBuffer(lines: _*)
    while (lb.length > 1) {
      val head = lb.head
      val tail = lb.tail.toList
      val (x0, ii) = closestFromIniToIni(head, tail)
      val (x1, ie) = closestFromIniToEnd(head, tail)
      val (x2, ei) = closestFromEndToIni(head, tail)
      val (x3, ee) = closestFromEndToEnd(head, tail)

      val minDistance = Seq(ii, ie, ei, ee).min

      if (minDistance < 1000) {

        val (original, added) = minDistance match {
          case u if u == ii => (x0, x0.reverse() ++ head)
          case u if u == ie => (x1, x1 ++ head)
          case u if u == ei => (x2, head ++ x2)
          case u if u == ee => (x3, head ++ x3.reverse())
          case _ =>
            throw new IllegalArgumentException(
              "value is not present in the array?"
            )
        }

        lb.remove(lb.indexWhere(_ == original))
        lb.remove(lb.indexWhere(_ == head))
        lb.append(added)
      } else {
        lb.remove(lb.indexWhere(_ == head))
        if (head.nodes.length > 100) {
          lb.append(head)
        } else {
          println(head.nodes.map(_.center).mkString(" , "))
        }
      }

    }

    lb.head
  }
}
