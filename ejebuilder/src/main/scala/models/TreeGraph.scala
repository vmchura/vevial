package models

import algorithms.DFS

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

      LinearGraph(reversedNodes.toList)
    }

  }


}
object TreeGraph{

  /**
    * Runs a DFS on any node, save the last visited node U, runs dfs on U,
    * the tree genereted by this DFS is returned
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




      var lastVisitedNode: A = nodesParam.head

      val dfsAlgoAnyNode = new DFS[A](a => lastVisitedNode = a,a => simpleEdges(indx(a)).toList)

      nodesParam.foreach(_.resetState())
      dfsAlgoAnyNode.run(lastVisitedNode)

      val treeNodes = ListBuffer.empty[A]
      val dfsAlgoALeaf= new DFS[A](treeNodes.append,a => simpleEdges(indx(a)).toList)
      nodesParam.foreach(_.resetState())
      dfsAlgoALeaf.run(lastVisitedNode)


      val edges = treeNodes.flatMap(a => simpleEdges(indx(a)).map(b => Edge(a, b)))

      new TreeGraph(treeNodes.toList, edges.toSet)
    }
  }



}
