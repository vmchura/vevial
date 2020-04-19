package models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, TPoint}

class GeoNode(val center: TPoint) extends TPoint with TGeoNode[GeoNode] {
  override val x: Double = center.x
  override val y: Double = center.y
}
