package models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, TPoint}

trait TGeoNode[A] extends TNode[A]{ self: A =>
  def center: TPoint
}
