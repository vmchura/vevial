package models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{PointUnitaryVector, TDirection, TPoint}

trait TEjeSectionEndPointsDefined {
  def in: PointUnitaryVector
  def out: PointUnitaryVector
  def points: Seq[TPoint]
}
