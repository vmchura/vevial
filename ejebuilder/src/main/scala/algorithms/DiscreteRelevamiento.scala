package algorithms

import com.typesafe.scalalogging.Logger
import io.vmchura.vevial.PlanarGeometric.BasicEje.SubsequenceFinder
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, TPoint}
import io.vmchura.vevial.elementdata.{TElementData, UPoint}
import io.vmchura.vevial.relevamiento.TSimpleRelevamiento
import models.{Edge, GeoNode, Graph, LinearGraph, TGeoNode}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object DiscreteRelevamiento {
  val logger = Logger(this.getClass)
  def convertIntoDiscreteRelevamiento[A <: TSimpleRelevamiento[B],B <: TElementData[B], N <: TGeoNode[N]](relevamientos: Seq[A]): Seq[LinearGraph[GeoNode]] = {

    logger.debug("Starting to build seq linear graph")

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
    val STEP_SAMPLE: Double = 10d
    val APROX_WIDTH_ROAD: Double = 6d
    val MAX_D: Double = Math.sqrt(APROX_WIDTH_ROAD*APROX_WIDTH_ROAD + STEP_SAMPLE*STEP_SAMPLE)
    val RADIUS: Double = MAX_D

    val (fX,fY) = List((extractorX,pointsSortedByX),(extractorY,pointsSortedByY)).map{case (e,orderedList) => (d: Double) =>
      SubsequenceFinder.find[PointWithUsing](RADIUS,RADIUS)(orderedList) (d) (e)} match {
      case first :: second :: Nil => (first,second)
      case _ => (null,null)
    }

    def mediaPoint(point: TPoint): (Point,Seq[PointWithUsing]) = {

      (for{
        (xIni,xEnd) <- fX(point.x)
        (yIni,yEnd) <- fY(point.y)
      }yield{

        val closeByX = pointsSortedByX.slice(xIni,xEnd+1)
        val closeByY = pointsSortedByY.slice(yIni,yEnd+1)

        val elementsAround = (closeByX intersect closeByY).filter(p => (!(p-point) < MAX_D))


        val n = elementsAround.length
        val xMedia = elementsAround.map(_.x).sum
        val yMedia = elementsAround.map(_.y).sum
        val mp = Point(xMedia/n,yMedia/n)
        (mp,elementsAround.toList)

      }).get

    }


    /**
      *
      * CREATE DRAFT NODES
      *
      * The idea
      * repeat until all points are used
      *   1.  Start with any point that are not used
      *   2.  find the media around that point, and create a node
      *   3.  mark those points as used.
      *
      *  after that, get a better nodes and build edges with the closest node
      */


    val nodesFirstDraft = points.flatMap{ q =>
      if(!q.used){
        val (mp,pointsFound) = mediaPoint(q)
        pointsFound.foreach(_.used = true)
        Some(mp)
      }else{
        None
      }
    }


    /**
      * after that, get a better nodes and build edges with the closest node
      */

    logger.debug(s"First nodes draft: ${nodesFirstDraft.length}")

    val nodes = nodesFirstDraft.map{ t =>
      val (mp,_) = mediaPoint(t)
      new GeoNode(mp)
    }



    val edges = nodes.zipWithIndex.flatMap{ case(n,i) =>
      nodes.drop(i).filter(m => (!(m-n)) < 2*MAX_D).sortBy(m => !(m-n)).headOption.map(m => List(Edge(n,m),Edge(m,n))).getOrElse(Nil)
    }.toSet

    logger.debug(s"Total Edges: ${edges.size}")

    val graph = new Graph(nodes.toList,edges)

    algorithms.BasicTreatment.buildTree(graph).map(_.toLinearGraph())


  }
}
