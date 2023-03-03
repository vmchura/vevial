package ScalaFXControllers

object MovingState extends Enumeration {
  type MovingState = Value
  val DraggingMap, DraggingNode, SelectionSquare, NotMoving = Value
}
