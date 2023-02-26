package Layers

import UtilTransformers.PointTransformer

trait LayerBuilder[T] {
  def build(pointTransformer: PointTransformer): TLayer[T]
}
