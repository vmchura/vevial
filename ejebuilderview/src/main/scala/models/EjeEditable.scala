package models

import Layers.{GeoNodeLayer, LinkLayer, SimpleEjeVialLayer}
import algorithms.EjeBuilderDraft
import io.vmchura.vevial.PlanarGeometric.EjeElement.TEjeElement

case class EjeEditable(initialGraph: LinearGraph[GeoNode],
                       geoNodeLayer: GeoNodeLayer,
                       ejeLayer: SimpleEjeVialLayer) extends TEjeEditable {

  override def geoNodeAdded(geoNode: GeoNode): Unit = geoNodeLayer.add(geoNode)

  override def geoNodeRemoved(geoNode: GeoNode): Unit = geoNodeLayer.remove(geoNode)

  override def elementAdded(e: TEjeElement): Unit = ejeLayer.add(e)

  override def elementRemoved(e: TEjeElement): Unit = ejeLayer.remove(e)

  override def clear(): Unit = {
    geoNodeLayer.clear()
    ejeLayer.clear()

  }
}
