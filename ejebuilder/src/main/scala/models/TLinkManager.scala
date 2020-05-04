package models

import algorithms.LinearEquationsSolver
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{PointUnitaryVector, TDirection, TPoint}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TDirection.AnyDirection
import io.vmchura.vevial.PlanarGeometric.EjeElement.TEjeElement

import scala.collection.mutable


trait TLinkManager{
  val geoNodeLinkMap: mutable.Map[TPoint,GeoLinkGraph] = mutable.Map.empty[TPoint,GeoLinkGraph]


  final def addGeoNodeLinkRelation(geoNode: TPoint, geoLinkGraph: GeoLinkGraph): Unit = {
    if(geoNodeLinkMap.contains(geoNode)){
      geoNodeLinkMap(geoNode) = geoLinkGraph
    }else{
      geoNodeLinkMap += geoNode -> geoLinkGraph
    }
  }



  protected def initialEjeElements: List[TEjeElement]
  protected def initialLinks: List[TLinkPoint]
  /*= {

    val nodes: Array[GeoNode] = initialGraph.nodes.toArray
    val links = buildLink(nodes,None,None)
    (links.flatMap(_.elements), links)
  }
  */




}

object TLinkManager{

  def buildDirections(nodes: Array[GeoNode],
                      left: Option[TDirection],
                      right: Option[TDirection]): Array[TDirection] = {
    val directions = Array.fill(nodes.length)(new DirectionAverage())
    nodes.length match {
      case i if i<=3 => throw  new IllegalArgumentException("not enough elements")
      case n => (2 until n).foreach{ i =>
        val (da,db,dc) = LinearEquationsSolver.calcDirections(nodes(i-2),nodes(i-1),nodes(i-0))
        if(!da.isInstanceOf[AnyDirection])
          directions(i-2).add(da)
        directions(i-1).add(db)
        if(!dc.isInstanceOf[AnyDirection])
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



      geoLink


    }

    links.zip(links.tail).foreach{ case (a,b) =>
      a.next = Some(b)
      b.prev = Some(a)
    }
    links.toList
  }
}
