package algorithms

import io.vmchura.vevial.PlanarGeometric.BasicEje.SubsequenceFinder
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, TPoint}
import io.vmchura.vevial.elementdata.{TElementData, UPoint}
import io.vmchura.vevial.relevamiento.TSimpleRelevamiento
import models.{Edge, GeoNode, Graph, LinearGraph, TGeoNode}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object DiscreteRelevamiento {
  def convertIntoDiscreteRelevamiento[A <: TSimpleRelevamiento[B],B <: TElementData[B], N <: TGeoNode[N]](relevamientos: Seq[A]): Seq[LinearGraph[GeoNode]] = {

    class PointWithUsing(point: Point) extends TPoint {
      var used: Boolean = false
      override val x: Double = point.x
      override val y: Double = point.y
    }
    val points: Array[PointWithUsing] = relevamientos.flatMap(_.elements.flatMap(_.point).map(p => new PointWithUsing(p.value))).toArray

    val pointsSortedByX = points.sortBy(_.x)
    val pointsSortedByY = points.sortBy(_.y)

    implicit val extractorX: PointWithUsing => Double = _.x
    implicit val extractorY: PointWithUsing => Double = _.y
    val RADIUS: Double = 40d
    val MAX_D: Double = Math.sqrt(6d*6d+10d*10d)
    val (fX,fY) = List((extractorX,pointsSortedByX),(extractorY,pointsSortedByY)).map{case (e,orderedList) => (d: Double) =>
      SubsequenceFinder.find[PointWithUsing](RADIUS,RADIUS)(orderedList) (d) (e)} match {
      case first :: second :: Nil => (first,second)
      case _ => (null,null)
    }
    val (fXAround,fYAround) = List((extractorX,pointsSortedByX),(extractorY,pointsSortedByY)).map{case (e,orderedList) => (d: Double) =>
      SubsequenceFinder.find[PointWithUsing](RADIUS*2,RADIUS*2)(orderedList) (d) (e)} match {
      case first :: second :: Nil => (first,second)
      case _ => (null,null)
    }

    def mediaPoint(point: PointWithUsing): (Point,Seq[PointWithUsing]) = {

      (for{
        (xIni,xEnd) <- fX(point.x)
        (yIni,yEnd) <- fY(point.y)
      }yield{

        val closeByX = pointsSortedByX.slice(xIni,xEnd+1)
        val closeByY = pointsSortedByY.slice(yIni,yEnd+1)

        val elementsAround = (closeByX intersect closeByY).filter(p => (!(p-point) < MAX_D)).sortBy(p => !(p-point)).take(4)


        val n = elementsAround.length
        val xMedia = elementsAround.map(_.x).sum
        val yMedia = elementsAround.map(_.y).sum
        val mp = Point(xMedia/n,yMedia/n)
        (mp,elementsAround.toList)

      }).get

    }

    def pointsFreeAround(point: TPoint): Seq[PointWithUsing] = {
      (for{
        (xIni,xEnd) <- fXAround(point.x)
        (yIni,yEnd) <- fYAround(point.y)
      }yield {
        val closeByX = pointsSortedByX.slice(xIni,xEnd+1).filter(!_.used)
        val closeByY = pointsSortedByY.slice(yIni,yEnd+1).filter(!_.used)

        (closeByX intersect closeByY).toList.filter(p => (!(p-point) < MAX_D)).sortBy(q => !(q-point))


      }).getOrElse(Nil)
    }


    /**
      * The idea
      * 1.  Start with any point that are not used
      * 2.  find the media around that point, and create a node
      * 3.  mark those points as used
      * 4.  starting from the media, look for close points not used
      * 5.  if point found, create a node, and connect to previous node
      */

    val pointsFree: mutable.Queue[PointWithUsing] = mutable.Queue(points: _*)

    val nodes = ListBuffer.empty[GeoNode]
    val edges = ListBuffer.empty[Edge[GeoNode]]

    val dfsAlgo = new DFS[GeoNode](_ => (),geoNode => {
      val pAround = pointsFreeAround(geoNode).take(1)

      val neigbours = pAround.flatMap{ p =>
        if(p.used){
          None
        }else{
          val (mp,pointsFound) = mediaPoint(p)
          pointsFound.foreach(_.used = true)
          Some(new GeoNode(mp))
        }
      }

      neigbours.foreach{ n =>
        edges.append(Edge(geoNode,n))
      }

      nodes.appendAll(neigbours)

      neigbours

    })


    while(pointsFree.nonEmpty){

      val pOpt = {
        pointsFree.dequeueWhile(_.used)
        if(pointsFree.nonEmpty)
          Some(pointsFree.dequeue())
        else
          None
      }
      pOpt.map{ point =>
        val (mp,pointsFound) = mediaPoint(point)
        if(pointsFound.length > 4)
          throw  new IllegalStateException("it only should be 4")
        val newNode = new GeoNode(mp)
        pointsFound.foreach(_.used = true)
        nodes.append(newNode)
        dfsAlgo.run(newNode)
      }
    }

    val graph = new Graph(nodes.toList,edges.toSet)

    algorithms.BasicTreatment.buildTree(graph).map(_.toLinearGraph())


  }
}
