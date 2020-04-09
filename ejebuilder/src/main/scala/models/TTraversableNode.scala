package models
import NodeState._
trait TTraversableNode[A] { self: A =>
  var level: Int = 0
  var state: NodeState = White
  var parent: A = this
  var deleted: Boolean = false
  def setParent(p: A): Unit = parent = p
  def unvisited(): Boolean = state == White
  def started(): Boolean = state == Grey
  def finished(): Boolean = state == Black

  def markStartTraverse(): Unit = state = Grey
  def markFinishTraverse(): Unit = state = Black
  def resetState(): Unit = {
    state = White
    level = 0
    deleted = false
  }
  def isDeleted(): Boolean = deleted
}
