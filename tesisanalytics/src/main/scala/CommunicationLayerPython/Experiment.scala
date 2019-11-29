package CommunicationLayerPython

import java.util.UUID

trait Experiment {
  def id: UUID
  def reset: Experiment
  def actions: Int
  def update(action: Int): Experiment
  def regard(action: Int): Double
  def finished: Boolean
}
