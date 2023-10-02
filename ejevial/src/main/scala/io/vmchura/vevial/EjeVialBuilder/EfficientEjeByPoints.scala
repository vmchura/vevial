package io.vmchura.vevial.EjeVialBuilder

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint

trait EfficientEjeByPoints {
  def findProgresiva(point: TPoint): Option[(Option[String], Double)]
}

object EfficientEjeByPoints{
  def sortByClosePoints(points: Seq[Seq[TPoint]]): Seq[TPoint] = {

  }
}
