package AutomaticBuilder.models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint

trait TElementCanImprove {
  def length: Double
  def calcProjection(p: TPoint): Option[TProjection]
}
