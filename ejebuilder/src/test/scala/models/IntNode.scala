package models

case class IntNode(v: Int) extends TNode[IntNode]{
  override def toString: String = v.toString
}