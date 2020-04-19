package models

import algorithms.LinearEquationsSolver
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TPoint}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{ElementPoint, TEjeElement}

import scala.collection.mutable.ListBuffer

trait TEjeEditable extends TLinkManager with TLinkUpdater{

  def geoNodeAdded(geoNode: GeoNode): Unit
  def geoNodeRemoved(geoNode: GeoNode): Unit
  def elementAdded(e: TEjeElement): Unit
  def elementRemoved(e: TEjeElement): Unit
  val mutableEje = new MutableEje(initialElementsGenerated)

  final override def removeElements: Seq[TEjeElement] => Unit = {elements: Seq[TEjeElement] =>
    elements.foreach(e => {
      elementRemoved(e)
      mutableEje.removeElement(e)
    })
  }
  final override def addElements: Seq[TEjeElement] => Unit = {elements: Seq[TEjeElement] =>
    elements.foreach(e => {
      elementAdded(e)
      mutableEje.addElement(e)
    })
  }

  def clear(): Unit




  def elementByPosition(point: Point): Option[Either[GeoNode,ElementPoint]] = {
    mutableEje.endPointsClosest(point) match {
      case Some(ElementPoint(a:GeoNode,_,_))  => Some(Left(a))
      case _ => mutableEje.projectPoint(point).map(ep => Right(ep))
    }

  }

  def createPairLinks(in: PointUnitaryVector, out: PointUnitaryVector, middle: GeoNode): (GeoLinkGraph,GeoLinkGraph) = {
    val (_,d,_) = LinearEquationsSolver.calcDirections(in.point,middle,out.point)

    val left = new GeoLinkGraph(in,PointUnitaryVector(middle,d))
    val right = new GeoLinkGraph(PointUnitaryVector(middle,d),out)

    addGeoNodeLinkRelation(middle,left)
    addGeoNodeLinkRelation(middle,right)

    in.point match {
      case x: GeoNode => addGeoNodeLinkRelation(x,left)
      case _ => throw new IllegalStateException("in is not geonode")
    }

    out.point match {
      case x: GeoNode => addGeoNodeLinkRelation(x,right)
      case _ => throw new IllegalStateException("out is not geonode")
    }


    left.next = Some(right)
    right.prev = Some(left)
    (left,right)
  }

  def createInnerNode(elementPoint: ElementPoint): GeoNode = {

    elementPoint.ejeElementOwner match {
      case s: TEjeElementTemporal => {
        val section = s.ejeSection

        val newGeoNode = new GeoNode(elementPoint.point)


        val (left,right) = createPairLinks(section.in,section.out,newGeoNode)


        updateSegment(section,section,left,right)
        val leftNth = left.prevNth(5)
        val rightNth = right.nextNth(5)
        val nodes = leftNth.nodesUntilTarget(rightNth).map(_.asInstanceOf[GeoNode])
        val link = buildLink(nodes.toArray,Some(leftNth.in.direction),Some(rightNth.out.direction))

        updateSegment(leftNth,rightNth,link.head,link.last)
        newGeoNode
      }
      case _ => throw new IllegalArgumentException("NOT A GEO LINK?")
    }


  }

  def moveGeoNodeTo(geoNode: GeoNode, destination: Point): Unit = {

    val newGeoNode = new GeoNode(destination)
    geoNodeLinkMap.get(geoNode) match {
      case Some(section) =>
        val in = section.in
        val out = section.out
        val prev = section.prev
        val next = section.next
        val (x,y) = if(in.point == geoNode){
          (prev,Some(section))
        }else{
          if(out.point == geoNode){
            (Some(section),next)
          }else{
            throw new IllegalArgumentException("Is not a correct END POINT")
          }
        }


        val toStart = (x,y) match {
          case (Some(a),Some(b)) =>
            val (left,right) = createPairLinks(a.in,b.out,newGeoNode)
            updateSegment(a,b,left,right)
            left
          case (Some(a),None) =>
            val newLink = new GeoLinkGraph(a.in,PointUnitaryVector(newGeoNode,a.in.direction))
            updateSegment(a,a,newLink,newLink)
            newLink
          case (None,Some(b)) =>
            val newLink = new GeoLinkGraph(PointUnitaryVector(newGeoNode,b.out.direction),b.out)
            updateSegment(b,b,newLink,newLink)
            newLink
          case _ => throw new IllegalStateException("At least some section needed")
        }
        val leftNth = toStart.prevNth(5)
        val rightNth = toStart.nextNth(5)
        val nodes = leftNth.nodesUntilTarget(rightNth).map(_.asInstanceOf[GeoNode])
        val link = buildLink(nodes.toArray,Some(leftNth.in.direction),Some(rightNth.out.direction))

        updateSegment(leftNth,rightNth,link.head,link.last)

      case None => throw  new IllegalStateException("GEO NODE NOT REGISTERED")

    }

    geoNodeAdded(newGeoNode)
    geoNodeRemoved(geoNode)


  }


}
