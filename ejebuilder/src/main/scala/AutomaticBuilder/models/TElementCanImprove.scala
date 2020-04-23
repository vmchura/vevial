package AutomaticBuilder.models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, TPoint}

trait TElementCanImprove {
  def length: Double
  def calcProjection(p: TPoint): Option[TProjection]
  def calcPointFromProjection(tProjection: TProjection): Option[Point]
}
