package io.vmchura.vevial.EjeVialBuilder

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint

trait EfficientEjeByPoints {
  def findProgresiva(point: TPoint): Option[Double]
}
