package models

import algorithms.LinearEquationsSolver
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{ElementPoint, TEjeElement}

import scala.collection.mutable.ListBuffer

trait TEjeEditable extends TLinkManager with TLinkUpdater[GeoNode]{

  def geoNodeAdded(geoNode: GeoNode): Unit
  def geoNodeRemoved(geoNode: GeoNode): Unit
  def elementAdded(e: TEjeElement): Unit
  def elementRemoved(e: TEjeElement): Unit
  final override def removeElements: Seq[TEjeElement] => Unit = {_.foreach(elementAdded)}
  final override def addElements: Seq[TEjeElement] => Unit = {_.foreach(elementRemoved)}

  def clear(): Unit


  val mutableEje = new MutableEje(initialElementsGenerated)


  def elementByPosition(point: Point): Option[Either[GeoNode,ElementPoint]] = {
    mutableEje.endPointsClosest(point) match {
      case Some(ElementPoint(a:GeoNode,_,_))  => Some(Left(a))
      case Some(_) => throw  new IllegalStateException("IS NOT GEO NODE!")
      case None => mutableEje.projectPoint(point).map(ep => Right(ep))
    }

  }

  def createPairLinks(in: PointUnitaryVector, out: PointUnitaryVector, middle: GeoNode): (GeoLinkGraph,GeoLinkGraph) = {
    val newGeoNode = new GeoNode(middle)
    val (_,d,_) = LinearEquationsSolver.calcDirections(in.point,newGeoNode,out.point)

    val left = GeoLinkGraph(in,PointUnitaryVector(newGeoNode,d))
    val right = GeoLinkGraph(PointUnitaryVector(newGeoNode,d),out)

    left.next = Some(right)
    right.prev = Some(left)
    (left,right)
  }

  def createInnerNode(elementPoint: ElementPoint): GeoNode = {
    elementPoint.ejeElementOwner match {
      case s: TEjeElementTemporal[GeoNode] => {
        val section = s.ejeSection

        val newGeoNode = new GeoNode(elementPoint.point)
        val (left,right) = createPairLinks(section.in,section.out,newGeoNode)
        updateSegment(section,section,left,right)
        newGeoNode
      }
      case _ => throw new IllegalArgumentException("NOT A GEO LINK?")
    }
  }

  def moveGeoNodeTo(geoNode: GeoNode, destination: Point): Unit = {

    val newGeoNode = new GeoNode(destination)
    mutableEje.endPointsClosest(geoNode) match {
      case Some(ElementPoint(_,_,link: GeoLinkGraph))  => {
        val GeoLinkGraph(in,out,prev,next) = link
        val (x,y) = if(in.point == geoNode){
          (prev,Some(link))
        }else{
          if(out.point == geoNode){
            (Some(link),next)
          }else{
            throw new IllegalArgumentException("Is not a correct END POINT")
          }
        }


        (x,y) match {
          case (Some(a),Some(b)) =>
            val (left,right) = createPairLinks(a.in,b.out,newGeoNode)
            updateSegment(a,b,left,right)
          case (Some(a),None) =>
            val newLink = GeoLinkGraph(a.in,PointUnitaryVector(newGeoNode,a.in.direction))
            updateSegment(a,a,newLink,newLink)
          case (None,Some(b)) =>
            val newLink = GeoLinkGraph(PointUnitaryVector(newGeoNode,b.out.direction),b.out)
            updateSegment(b,b,newLink,newLink)
          case _ => throw new IllegalStateException("At least some section needed")
        }


      }
      case Some(_) => throw  new IllegalStateException("IS NOT GEO NODE!")
      case None => throw  new IllegalStateException("IT IS NOT ON THE RIGHT POSITION")
    }
    geoNodeAdded(newGeoNode)
    geoNodeRemoved(geoNode)
  }



}
