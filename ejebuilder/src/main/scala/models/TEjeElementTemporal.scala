package models

import algorithms.LinearEquationsSolver
import algorithms.LinearEquationsSolver.{buildCircleSegment, buildCircleTangent}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{PlanarVector, Point, PointUnitaryVector, TDirection, TPoint}
import io.vmchura.vevial.PlanarGeometric.EjeElement._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


trait TLinkPoint {
  def in: PointUnitaryVector
  def out: PointUnitaryVector
  def next: Option[TLinkPoint]
  def next_= (newNext: Option[TLinkPoint]): Unit
  def prev: Option[TLinkPoint]
  def prev_= (newPrev: Option[TLinkPoint]): Unit
  def elements: Seq[TEjeElementTemporal]
  final def untilTarget(target: TLinkPoint): List[TLinkPoint] = {
    val seqElements = ListBuffer.empty[TLinkPoint]
    var x: Option[TLinkPoint] = Some(this)
    while(x.isDefined){
      val t = x.get

      seqElements.append(t)

      if(t == target){
        x = None
      }else{
        x =t.next
      }
    }

    seqElements.toList
  }
  final def nodesUntilTarget(target: TLinkPoint): List[TPoint] = {
    val seqElements = ListBuffer[TPoint](in.point)
    var x: Option[TLinkPoint] = Some(this)

    while(x.isDefined){
      val t = x.get

      seqElements.append(t.out.point)

      if(t == target){
        x = None
      }else{
        x =t.next
      }
    }

    seqElements.toList
  }
  final def prevNth(n: Int): TLinkPoint = {
    if(n == 0)
      this
    else{
      prev.map(_.prevNth(n-1)).getOrElse(this)
    }
  }
  final def nextNth(n: Int): TLinkPoint = {
    if(n == 0)
      this
    else{
      next.map(_.nextNth(n-1)).getOrElse(this)
    }
  }
}

sealed trait TEjeElementTemporal extends TEjeElement{
  def ejeSection: TLinkPoint
}

case class FaintTemporal(from: TPoint, end: TPoint, ejeSection: TLinkPoint) extends TFaintElement with TEjeElementTemporal
case class RectTemporal(originPoint: TPoint, endPoint: TPoint, ejeSection: TLinkPoint) extends TRectSegment with TEjeElementTemporal
case class CircleTemporal(originPoint: TPoint, centerPoint: TPoint, endPoint: TPoint, antiClockWise: Boolean, ejeSection: TLinkPoint) extends TCircleSegment with TEjeElementTemporal

class GeoLinkGraph(val in: PointUnitaryVector,val out: PointUnitaryVector,
                        var prev: Option[TLinkPoint]=None, var next: Option[TLinkPoint]=None
                       ) extends TLinkPoint {

  override val elements: Seq[TEjeElementTemporal] = {
    LinearEquationsSolver.buildCircleTangent(in,out) match {
      case Some(x) => {
        val c = CircleTemporal(x.originPoint,x.centerPoint,x.endPoint,x.antiClockWise,this)
        val left = if(c.originPoint ==? in.point) None else Some(RectTemporal(in.point,c.originPoint,this))
        val right = if(c.endPoint ==? out.point) None else Some(RectTemporal(c.endPoint,out.point,this))
        List(left,Some(c),right).flatten
      }
      case None => List(FaintTemporal(in.point,out.point,this))
    }
  }


}

trait TLinkUpdater{
  def removeElements: Seq[TEjeElement] => Unit
  def addElements: Seq[TEjeElement] => Unit
  final def updateSegment[A <: TPoint](oldLinkBegin: TLinkPoint, oldLinkEnd: TLinkPoint,
                    newLinkBegin: TLinkPoint, newLinkEnd: TLinkPoint): Unit = {
    val elementsToDrop = oldLinkBegin.untilTarget(oldLinkEnd).flatMap(_.elements)
    println(s"#Elements to drop: ${elementsToDrop.length}")

    removeElements(elementsToDrop)

    val elementsToAdd = newLinkBegin.untilTarget(newLinkEnd).flatMap(_.elements)
    addElements(elementsToAdd)
    println(s"#Elements to add: ${elementsToAdd.length}")

    oldLinkBegin.prev match {
      case Some(prev) =>
        prev.next = Some(newLinkBegin)
        newLinkBegin.prev = Some(prev)
      case None =>
        newLinkBegin.prev = None
    }

    oldLinkEnd.next match {
      case Some(next) =>
        next.prev = Some(newLinkEnd)
        newLinkEnd.next = Some(next)
      case None =>
        newLinkEnd.next = None
    }


  }
}

trait TLinkManager{
  def initialGraph: LinearGraph[GeoNode]
  val geoNodeLinkMap: mutable.Map[GeoNode,GeoLinkGraph] = mutable.Map.empty[GeoNode,GeoLinkGraph]

  final def addGeoNodeLinkRelation(geoNode: GeoNode, geoLinkGraph: GeoLinkGraph): Unit = {
    if(geoNodeLinkMap.contains(geoNode)){
      geoNodeLinkMap(geoNode) = geoLinkGraph
    }else{
      geoNodeLinkMap += geoNode -> geoLinkGraph
    }
  }




  final def initialElementsGenerated: List[TEjeElement] = {
    val nodes: Array[GeoNode] = initialGraph.nodes.toArray

    val links = buildLink(nodes,None,None)
    links.flatMap(_.elements)


  }

  def buildDirections(nodes: Array[GeoNode],
                      left: Option[TDirection],
                      right: Option[TDirection]): Array[TDirection] = {
    val directions = Array.fill(nodes.length)(new DirectionAverage())
    nodes.length match {
      case i if i<=3 => throw  new IllegalArgumentException("not enough elements")
      case n => (2 until n).foreach{ i =>
        val (da,db,dc) = LinearEquationsSolver.calcDirections(nodes(i-2),nodes(i-1),nodes(i-0))
        directions(i-2).add(da)
        directions(i-1).add(db)
        directions(i-0).add(dc)
      }

    }
    left.foreach{ leftDirection =>
      directions(0) = {
        val d = new DirectionAverage()
        d.add(leftDirection)
        d
      }
    }

    right.foreach{ rightDirection =>
      directions(nodes.length-1) = {
        val d = new DirectionAverage()
        d.add(rightDirection)
        d
      }

    }

    directions.map(_.value())
  }

  def buildLink(nodes: Array[GeoNode],
                left: Option[TDirection],
                right: Option[TDirection]): List[GeoLinkGraph] = {
    val directions = buildDirections(nodes,left,right)

    val links = (1 until nodes.length).map{ i =>
      val geoLink = new GeoLinkGraph(PointUnitaryVector(nodes(i-1),directions(i-1)),
        PointUnitaryVector(nodes(i),directions(i))
      )

      addGeoNodeLinkRelation(nodes(i-1),geoLink)
      addGeoNodeLinkRelation(nodes(i),geoLink)

      geoLink


    }

    links.zip(links.tail).foreach{ case (a,b) =>
      a.next = Some(b)
      b.prev = Some(a)
    }
    links.toList
  }

}

class DirectionAverage(){
  val allValues = ListBuffer.empty[PlanarVector]
  def add(t: TDirection): Unit = allValues += PlanarVector(t.direction,1)
  def value(): TDirection  = allValues.reduceLeft(_ + _).direction

}