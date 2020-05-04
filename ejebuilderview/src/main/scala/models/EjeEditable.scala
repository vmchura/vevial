package models

import AutomaticBuilder.models.TElementCanImprove
import Layers.{GeoNodeLayer, LinkLayer, SimpleEjeVialLayer}
import algorithms.EjeBuilderDraft
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import io.vmchura.vevial.PlanarGeometric.EjeElement.TEjeElement

class EjeEditable private (val initialEjeElements: List[TEjeElement],val initialLinks: List[TLinkPoint],
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

  addElements(initialEjeElements)
  initialLinks.foreach(l => addGeoNode(l.out.point.asInstanceOf[GeoNode]))
  addGeoNode(initialLinks.head.in.point.asInstanceOf[GeoNode])

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

object EjeEditable{

  def apply(initialEjeElements: List[TEjeElement], initialLinks: List[TLinkPoint])(geoNodeLayer: GeoNodeLayer, ejeLayer: SimpleEjeVialLayer, centerWindow: TPoint => Unit): EjeEditable ={
    assert(initialLinks.forall(link => link.in.point.isInstanceOf[GeoNode] && link.out.point.isInstanceOf[GeoNode]))
    new EjeEditable(initialEjeElements, initialLinks, geoNodeLayer, ejeLayer, centerWindow)
  }
  def apply(initialGraph: LinearGraph[GeoNode])(geoNodeLayer: GeoNodeLayer, ejeLayer: SimpleEjeVialLayer, centerWindow: TPoint => Unit): EjeEditable = {


      val nodes: Array[GeoNode] = initialGraph.nodes.toArray
      val links = TLinkManager.buildLink(nodes,None,None)
      val res = new EjeEditable(links.flatMap(_.elements),links,geoNodeLayer,ejeLayer,centerWindow)
      links.foreach{ link =>
        res.addGeoNodeLinkRelation(link.in.point,link)
      }

      res
  }
}