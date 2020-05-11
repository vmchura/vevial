package models

import AutomaticBuilder.models.{ProjectionOverElement, TElementCanImprove, TProjection}
import algorithms.LinearEquationsSolver
import algorithms.LinearEquationsSolver.{buildCircleSegment, buildCircleTangent}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TDirection.AnyDirection
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{PlanarVector, Point, PointUnitaryVector, TDirection, TPoint}
import io.vmchura.vevial.PlanarGeometric.EjeElement._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


trait TLinkPoint extends TElementCanImprove{
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

  final def untilEnd(): List[TLinkPoint] = {
    val seqElements = ListBuffer.empty[TLinkPoint]
    var x: Option[TLinkPoint] = Some(this)
    while(x.isDefined){
      val t = x.get

      seqElements.append(t)


        x =t.next

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
  override final def calcPointFromProjection(tProjection: TProjection): Option[Point] = {
    elements.scanLeft((tProjection.distanceOverElement,Option.empty[Point])){
      case ((lengthToFit,_),e) => (lengthToFit - e.length,e.pointFromProjection(ProjectionOverElement(lengthToFit,tProjection.distanceNormal)))
    }.flatMap(_._2).headOption

  }
  def calcTangent(lengthOverElement: Double): Option[TDirection] = {
    elements.scanLeft((lengthOverElement,Option.empty[TDirection])){
      case ((lengthToFit,_),e) => {
        if(e.length >= lengthToFit){
          val direc = e.pointFromProjection(ProjectionOverElement(lengthToFit,0d)).map(p =>
            e.tangent(p)
          )
          (lengthToFit-e.length,direc)

        }else{
          (lengthToFit-e.length,None)
        }

      }
    }.flatMap(_._2).headOption
  }

  def pointsDataCovering: Iterable[TPoint]
  def addPointCovered(tpoint: TPoint): Unit


  final def firstDefined(): TLinkPoint = {
    var x = this
    while(x.prev.isDefined)
      x = x.prev.get

    x
  }

}



