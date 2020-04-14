package Layers

import scalafx.collections.ObservableBuffer
import scalafx.scene.Node

trait ObservableNodes {
  val nodes: ObservableBuffer[Node] = new ObservableBuffer[Node]()
}
