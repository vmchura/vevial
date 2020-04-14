package models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point

class GeoNode(override val center: Point) extends Point(center.x,center.y) with TGeoNode[GeoNode] {

}
