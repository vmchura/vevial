package models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point
import io.vmchura.vevial.PlanarGeometric.EjeElement.{ElementPoint, TRectSegment}

import scala.collection.mutable.ListBuffer

trait TLinearGraphEditable{

  def intialElements: Seq[GeoNode]
  def geoNodesUpdated(): Unit


  def geoNodeAdded(geoNode: GeoNode): Unit
  def geoNodeRemoved(geoNode: GeoNode): Unit
  def linkAdded(linkGeo: LinkGeo): Unit
  def linkRemoved(linkGeo: LinkGeo): Unit


  val nodes = ListBuffer(intialElements: _*)
  intialElements.foreach(geoNodeAdded)
  println(s"#geoNodes: ${intialElements.length}")
  val links = ListBuffer(intialElements.zip(intialElements.tail).map{case (a,b) => {
    val link = LinkGeo(a,b)
    link
  }}: _*)
  links.foreach(linkAdded)

  final def clear(): Unit = {
    links.foreach(linkRemoved)
    nodes.foreach(geoNodeRemoved)
  }

  val mutableByLink = new MutableEje(links.toList)


  def setGraph(linearGraph: LinearGraph[GeoNode]): Unit = {
    nodes.clear()
    nodes.appendAll(linearGraph.nodes)
  }

  def elementByPosition(point: Point): Option[Either[GeoNode,ElementPoint]] = {
    nodes.find(g => (!(g.center - point)) <= 2) match {
      case Some(g) => Some(Left(g))
      case None => mutableByLink.projectPoint(point).map(ep => Right(ep))
    }
  }

  def createInnerNode(elementPoint: ElementPoint): GeoNode = {
    elementPoint.ejeElementOwner match {
      case LinkGeo(from,to) => {
        val newGeoNode = new GeoNode(elementPoint.point)

        val indx = nodes.indexWhere(_ == from)
        nodes.insert(indx+1,newGeoNode)

        mutableByLink.removeElement(elementPoint.ejeElementOwner)
        links.remove(links.indexWhere(_ == elementPoint.ejeElementOwner))

        val link0 = LinkGeo(from,newGeoNode)
        val link1 = LinkGeo(newGeoNode,to)
        mutableByLink.addElement(link0)
        mutableByLink.addElement(link1)
        links.append(link0)
        links.append(link1)

        geoNodeAdded(newGeoNode)
        linkAdded(link0)
        linkAdded(link1)
        linkRemoved(elementPoint.ejeElementOwner.asInstanceOf[LinkGeo])
        newGeoNode
      }
      case _ => throw new IllegalArgumentException("NOT A GEO LINK?")
    }
  }

  def moveGeoNodeTo(geoNode: GeoNode, destination: Point): Unit = {

    val newGeoNode = new GeoNode(destination)

    def updateLink(oldLink: LinkGeo, newLink: LinkGeo): Unit = {
      links.remove(links.indexWhere(_ == oldLink))
      mutableByLink.removeElement(oldLink)

      mutableByLink.addElement(newLink)
      links.append(newLink)

      linkAdded(newLink)
      linkRemoved(oldLink)

    }
    links.find(_.originPoint == geoNode).foreach{ link =>
      val LinkGeo(_,to) = link
      updateLink(link,LinkGeo(newGeoNode,to))

    }

    links.find(_.endPoint == geoNode).foreach{ link =>
      val LinkGeo(from,_) = link
      updateLink(link,LinkGeo(from,newGeoNode))
    }


    val indx = nodes.indexWhere(_ == geoNode)
    nodes.update(indx,newGeoNode)
    geoNodeAdded(newGeoNode)
    geoNodeRemoved(geoNode)
  }



}
