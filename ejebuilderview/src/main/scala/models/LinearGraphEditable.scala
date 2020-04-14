package models

import Layers.{GeoNodeLayer, LinkLayer}

case class LinearGraphEditable(intialElements: Seq[GeoNode], linkLayer: LinkLayer, geoNodeLayer: GeoNodeLayer) extends TLinearGraphEditable {

  override def geoNodeAdded(geoNode: GeoNode): Unit = geoNodeLayer.add(geoNode)

  override def geoNodeRemoved(geoNode: GeoNode): Unit = geoNodeLayer.remove(geoNode)

  override def linkAdded(linkGeo: LinkGeo): Unit = linkLayer.add(linkGeo)

  override def linkRemoved(linkGeo: LinkGeo): Unit = linkLayer.remove(linkGeo)
}
