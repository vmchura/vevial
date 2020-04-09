package models

trait TGraph[A <: TNode[A]] {
  def nodes: Seq[A]
  def edges: Set[Edge[A]]
  def n = nodes.length
}
