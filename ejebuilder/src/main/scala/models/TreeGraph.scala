package models

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class TreeGraph[A <: TNode[A]] private (nodes: Seq[A],edges: Set[Edge[A]]) extends TGraph[A]{

  /**
    * It returns the diameter
    * @return
    */
  def toLinearGraph(): LinearGraph[A] = {
    if(nodes.isEmpty){
      LinearGraph[A](Nil)
    }else {
      val indx = nodes.zipWithIndex.map { case (nod, i) => nod -> i }.toMap
      val simpleEdges = Array.fill(n)(ListBuffer.empty[A])
      edges.foreach {
        case Edge(from, to) =>
          simpleEdges(indx(from)).append(to)
          simpleEdges(indx(to)).append(from)
      }

      def bfs(a: A)(implicit atStart: A => Unit): Unit = {

        val queue = mutable.Queue(a)

        while (queue.nonEmpty) {
          val x = queue.dequeue()
          if (x.unvisited()) {
            x.markStartTraverse()
            atStart(x)
            val adj = simpleEdges(indx(x))
            adj.foreach { v =>
              if (v.unvisited()) {
                queue.enqueue(v)
                v.setParent(x)
              }
            }
          }
          x.markFinishTraverse()
        }


      }

      var lastU = nodes.head

      nodes.foreach(_.resetState())
      bfs(nodes.head)(x => lastU = x)


      var lastV = nodes.head
      nodes.foreach(_.resetState())
      bfs(lastU)(x => lastV = x)


      val reversedNodes = ListBuffer.empty[A]
      var x = lastV
      while (x != lastU) {
        reversedNodes.append(x)
        x = x.parent
      }
      reversedNodes.append(x)
      println(lastV)
      println(reversedNodes.take(5).mkString(" - "))
      LinearGraph(reversedNodes.toList)
    }

  }


}
object TreeGraph{

  /**
    * Runs a DFS on any node, save the last visited node U, runs dfs on U, the tree genereted by this DFS is returned
    * @param nodesParam
    * @param edgesParam
    * @tparam A
    * @return
    */
  def apply[A <: TNode[A]](nodesParam: Seq[A], edgesParam: Set[Edge[A]]): TreeGraph[A] = {
    if(nodesParam.isEmpty){
      new TreeGraph[A](Nil,Set.empty)
    }else {
      val n = nodesParam.length
      val indx = nodesParam.zipWithIndex.map { case (nod, i) => nod -> i }.toMap

      val simpleEdges = Array.fill(n)(ListBuffer.empty[A])
      edgesParam.foreach {
        case Edge(from, to) =>
          simpleEdges(indx(from)).append(to)
          simpleEdges(indx(to)).append(from)
      }

      def dfs(a: A)(atStart: A => Unit, atEnd: A => Unit): Unit = {
        a.markStartTraverse()
        atStart(a)


        val adj = simpleEdges(indx(a))
        adj.foreach { v =>
          if (v.unvisited()) {
            dfs(v)(atStart, atEnd)
          }
        }
        a.markFinishTraverse()
        atEnd(a)
      }

      var lastVisitedNode: A = nodesParam.head
      val atStart1: A => Unit = _ => ()
      val atEnd1: A => Unit = x => lastVisitedNode = x

      nodesParam.foreach(_.resetState())
      dfs(nodesParam.head)(atStart1, atEnd1)

      val treeNodes = ListBuffer.empty[A]

      val atStart2: A => Unit = treeNodes.append
      val atEnd2: A => Unit = _ => ()

      nodesParam.foreach(_.resetState())
      dfs(lastVisitedNode)(atStart2, atEnd2)


      val edges = nodesParam.flatMap(a => simpleEdges(indx(a)).map(b => Edge(a, b)))

      new TreeGraph(nodesParam, edges.toSet)
    }
  }



}
