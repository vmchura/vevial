package models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point

trait TGeoNode[A] extends TNode[A]{ self: A =>
  def center: Point
}
