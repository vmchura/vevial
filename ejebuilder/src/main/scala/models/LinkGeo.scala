package models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point
import io.vmchura.vevial.PlanarGeometric.EjeElement.TRectSegment

case class LinkGeo(originPoint: GeoNode, endPoint: GeoNode) extends  TRectSegment {
  //override val originPoint: Point = from.center
  //override val endPoint: Point = to.center
}