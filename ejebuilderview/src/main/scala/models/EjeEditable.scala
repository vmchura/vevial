package models

import AutomaticBuilder.models.TElementCanImprove
import Layers.{GeoNodeLayer, LinkLayer, SimpleEjeVialLayer}
import algorithms.EjeBuilderDraft
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import io.vmchura.vevial.PlanarGeometric.EjeElement.TEjeElement

case class EjeEditable(initialGraph: LinearGraph[GeoNode],
                       geoNodeLayer: GeoNodeLayer,
                       ejeLayer: SimpleEjeVialLayer, centerWindow: TPoint => Unit) extends TEjeEditable {

  override def geoNodeAdded(geoNode: GeoNode): Unit = geoNodeLayer.add(geoNode)

  override def geoNodeRemoved(geoNode: GeoNode): Unit = geoNodeLayer.remove(geoNode)

  override def elementAdded(e: TEjeElement): Unit = ejeLayer.add(e)


  override def elementRemoved(e: TEjeElement): Unit = ejeLayer.remove(e)

  override def clear(): Unit = {
    geoNodeLayer.clear()
    ejeLayer.clear()

  }

  addElements(elementsToObserve)
  initialGraph.nodes.foreach(geoNodeAdded)

  /**
    * changes the state previous to the next upgrade
    *   - ex. visually move the window to the next point
    *   - ex. automaticale, does not do anything
    */
  override def locateUpgrade(element: TElementCanImprove): Unit = {
    element match {
      case t: TLinkPoint => centerWindow(t.in.point -%- t.out.point)
      case _ => ()
    }

  }
}
