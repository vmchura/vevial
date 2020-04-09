package models

import scala.collection.mutable.ListBuffer

case class TreeGraph[A <: TNode[A]](nodes: Seq[A],edges: Set[Edge[A]]) extends TGraph[A]{
  def toLinearGraph(): Option[LinearGraph[A]] = {
    val indx = nodes.zipWithIndex.map{case (nod,i) => nod -> i}.toMap
    val count = Array.fill(n)(0)
    edges.foreach{
      case Edge(from,_) =>
        count(indx(from)) += 1
    }

    if((count.count(_ == 1) == 2) && (count.count(_ == 2) == (n-2))){
      val simpleEdges = Array.fill(n)(ListBuffer.empty[A])
      edges.foreach{
        case Edge(from,to) =>
          simpleEdges(indx(from)).append(to)
          simpleEdges(indx(to)).append(from)
      }

      def dfs(a: A)(implicit atStart: A => Unit): Unit = {
        a.markStartTraverse()
        atStart(a)

        val adj = simpleEdges(indx(a))

        adj.foreach{ v =>
          if(v.unvisited()){
            dfs(v)
          }



        }
        a.markFinishTraverse()
      }

      val orderedNodes = ListBuffer.empty[A]
      implicit val atStart: A => Unit = orderedNodes.append

      dfs(nodes(count.find(_ == 1).get))

      Some(LinearGraph(orderedNodes.toList))
    }else{
      None
    }

  }
}
object TreeGraph{

  def apply[A <: TNode[A]](nodesParam: Seq[A], haveDirectConecction: (A,A) => Boolean): TreeGraph[A] = {
    val nodes: Seq[A] = nodesParam
    val edges: Set[Edge[A]] = {
      nodesParam.zip(nodesParam.tail).flatMap{ case (x,y) =>
        if(haveDirectConecction(x,y))
          Some(Set(Edge(x,y),Edge(y,x)))
        else
          None
      }.flatten
    }.toSet
    new TreeGraph(nodes,edges)
  }



}
