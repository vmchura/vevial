package models

import algorithms.LinearEquationsSolver
import algorithms.LinearEquationsSolver.{buildCircleSegment, buildCircleTangent}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{PlanarVector, Point, PointUnitaryVector, TDirection, TPoint}
import io.vmchura.vevial.PlanarGeometric.EjeElement._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


trait TLinkPoint[A <: TPoint] {
  def in: PointUnitaryVector
  def out: PointUnitaryVector
  def next: Option[TLinkPoint[A]]
  def next_= (newNext: Option[TLinkPoint[A]]): Unit
  def prev: Option[TLinkPoint[A]]
  def prev_= (newPrev: Option[TLinkPoint[A]]): Unit
  def elements: Seq[TEjeElementTemporal[A]]
  final def untilTarget(target: TLinkPoint[A]): List[TLinkPoint[A]] = {
    val seqElements = ListBuffer.empty[TLinkPoint[A]]
    var x: Option[TLinkPoint[A]] = Some(this)
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
}

sealed trait TEjeElementTemporal[A <: TPoint] extends TEjeElement{
  def ejeSection: TLinkPoint[A]
}

case class FaintTemporal[A](from: Point, end: Point, ejeSection: TLinkPoint[A]) extends TFaintElement with TEjeElementTemporal[A]
case class RectTemporal[A](originPoint: Point, endPoint: Point, ejeSection: TLinkPoint[A]) extends TRectSegment with TEjeElementTemporal[A]
case class CircleTemporal[A](originPoint: Point, centerPoint: Point, endPoint: Point, antiClockWise: Boolean, ejeSection: TLinkPoint[A]) extends TCircleSegment with TEjeElementTemporal[A]

case class GeoLinkGraph(in: PointUnitaryVector,out: PointUnitaryVector,
                        var prev: Option[TLinkPoint[GeoNode]]=None, var next: Option[TLinkPoint[GeoNode]]=None
                       ) extends TLinkPoint[GeoNode] {

  override val elements: Seq[TEjeElementTemporal[GeoNode]] = {
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

trait TLinkUpdater[A]{
  def removeElements: Seq[TEjeElement] => Unit
  def addElements: Seq[TEjeElement] => Unit
  final def updateSegment(oldLinkBegin: TLinkPoint[A], oldLinkEnd: TLinkPoint[A],
                    newLinkBegin: TLinkPoint[A], newLinkEnd: TLinkPoint[A]): Unit = {
    val elementsToDrop = oldLinkBegin.untilTarget(oldLinkEnd).flatMap(_.elements)
    removeElements(elementsToDrop)

    val elementsToAdd = newLinkBegin.untilTarget(newLinkEnd).flatMap(_.elements)
    addElements(elementsToAdd)

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
  final def initialElementsGenerated: List[TEjeElement] = {
    val nodes = initialGraph.nodes.toArray
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

    val links = (1 until nodes.length).map{ i =>
      GeoLinkGraph(PointUnitaryVector(nodes(i-1),directions(i-1).value()),
        PointUnitaryVector(nodes(i),directions(i).value())
      )
    }

    links.zip(links.tail).foreach{ case (a,b) =>
      a.next = Some(b)
      b.prev = Some(a)
    }

    links.flatMap(_.elements).toList


  }







}

class DirectionAverage(){
  val allValues = ListBuffer.empty[PlanarVector]
  def add(t: TDirection): Unit = allValues += PlanarVector(t.direction,1)
  def value(): TDirection  = allValues.reduceLeft{case (a,b) => a + b}.direction

}