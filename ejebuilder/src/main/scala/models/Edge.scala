package models

case class Edge[A <: TNode[A]](from: A, to: A){
  override def toString: String = s"$from -> $to"


}
