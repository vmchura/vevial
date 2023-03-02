package Layers

import UtilTransformers.PointTransformer

trait LayerBuilder[T] {
  def build(pointTransformer: PointTransformer): TLayer[T]
  def minimumX: Double
  def minimumY: Double

  def maximumX: Double

  def maximumY: Double
}
