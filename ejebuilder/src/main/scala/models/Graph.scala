package models

case class Graph[A <: TNode[A]](nodes: Seq[A],edges: Seq[Edge[A]]){
  override def toString: String = {
    val nStr = nodes.mkString(" ")
    val eStr = edges.mkString("\n")
    s"nodes: $nStr\nedges: \n$eStr"
  }
}

object Graph{
  def apply[A <: TNode[A]](nodesParam: Seq[A], haveDirectConecction: (A,A) => Boolean): Graph[A] = {
    val nodes: Seq[A] = nodesParam
    val edges: Seq[Edge[A]] = {
      nodesParam.zip(nodesParam.tail).flatMap{ case (x,y) =>
        if(haveDirectConecction(x,y))
          Some(List(Edge(x,y),Edge(y,x)))
        else
          None
      }.flatten
    }
    new Graph(nodes,edges)
  }
}