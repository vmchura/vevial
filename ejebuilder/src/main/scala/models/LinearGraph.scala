package models

case class LinearGraph[A <: TNode[A]](nodes: Seq[A]) extends TGraph[A]{
  override def edges: Set[Edge[A]] = throw new NotImplementedError()
}

