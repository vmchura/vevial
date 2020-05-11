package algorithms

import java.io.File

import AutomaticBuilder.models.{ProjectionOverElement, TElementCanImprove}
import io.vmchura.vevial.EjeVialBuilder.TConvertibleToEje
import io.vmchura.vevial.EjeVialUtil.Progresiva
import io.vmchura.vevial.PlanarGeometric.BasicEje.EfficientSeqEjeElements
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TDirection.{AnyDirection, Direction}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{PlanarVector, Point, PointUnitaryVector, TDirection, TPoint}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{ElementPoint, TEjeElement}
import io.vmchura.vevial.elementdata.IRIElementData
import io.vmchura.vevial.models.RelevamientoIRIProgresivas
import io.vmchura.vevial.relevamiento.RelevamientoIRI
import models.{GeoLinkGraph, GeoNode, TEjeEditable, TLinkPoint}
import models.extensioneje.EjeFromLinks

object MaximizeEje {

  class EjeEditableTemporal(val initialEjeElements: List[TEjeElement],val initialLinks: List[TLinkPoint]) extends TEjeEditable{
    override protected def geoNodeAdded(geoNode: GeoNode): Unit = ()

    override protected def geoNodeRemoved(geoNode: GeoNode): Unit = ()

    override def elementAdded(e: TEjeElement): Unit = ()

    override def elementRemoved(e: TEjeElement): Unit = ()

    override def clear(): Unit = ()

    /**
      * changes the state previous to the next upgrade
      *   - ex. visually move the window to the next point
      *   - ex. automaticale, does not do anything
      */
    override def locateUpgrade(element: TElementCanImprove): Unit = ()

    addElements(initialEjeElements)
    initialLinks.foreach(l => addGeoNode(l.out.point.asInstanceOf[GeoNode]))
    addGeoNode(initialLinks.head.in.point.asInstanceOf[GeoNode])

  }
  def apply(files: Seq[File], headLink: TLinkPoint): Either[Seq[Exception],TLinkPoint] = {
    val links = headLink.untilEnd()
    val eje = new EjeEditableTemporal(links.flatMap(_.elements),links)
    val relevamientos = files.flatMap(f => {
      val rel: RelevamientoIRI[IRIElementData] = RelevamientoIRI(f, cd => IRIElementData(cd))
      rel.elements.flatMap(_.point).map(_.value)
    })

    eje.setInitialPointsFree(relevamientos)

    val delta = for {
      dx <- Range.BigDecimal(-2d, 2d, 0.5d)
      dy <- Range.BigDecimal(-2d, 2d, 0.5d)
    }yield{
      val p = Point(dx.doubleValue,dy.doubleValue)
      val v = (p -? Point(0d,0d)).getOrElse(PlanarVector(TDirection(),0d))
      v
    }
    val deltaAngle = Range.BigDecimal(-Math.PI,Math.PI,2*Math.PI/30d)
    val fullDeltaAngle = Range.BigDecimal(-Math.PI,Math.PI,Math.PI/60d)
    val initialIn = PointUnitaryVector(new GeoNode(headLink.in.point),headLink.in.direction)
    val (_,pointsNotUsed,linksFinal) = links.foldLeft((initialIn,List.empty[TPoint],List.empty[TLinkPoint])){
      case ((in,pointsPrev,finalList),currentLinkDraft) => {
        val points = pointsPrev ++ currentLinkDraft.pointsDataCovering
        val u = (for{
          d <- delta
          alpha <- currentLinkDraft.out.direction match {
            case dir: Direction => deltaAngle.map(d => dir << d.doubleValue)
            case _: AnyDirection => fullDeltaAngle.map(d => TDirection(1,0) << d.doubleValue)
          }
        }yield{
          val np = new GeoNode(currentLinkDraft.out.point + d)
          val out = PointUnitaryVector(np,alpha)
          try{

            val e = (new GeoLinkGraph(in,out)).elements
            calcError(e,points).map { error => (error, out)
            }
          }catch{
            case _: Throwable => None
          }
        }).flatten.minByOption(_._1)

        u match {
          case Some((_,out)) =>
            val newLink = new GeoLinkGraph(in, out)

            if(points.length > 10) {
              try {
                val projections = points.flatMap(p => newLink.calcProjection(p).map(q => (q, p))).sortBy(_._1.distanceOverElement)
                val n = projections.length
                val (pointsDrop, pointstoMantain) = projections.splitAt(n / 2)
                val lengthAvance = pointsDrop.last._1.distanceOverElement
                println(s"Avance: $lengthAvance")
                (for {
                  t <- newLink.calcTangent(lengthAvance)
                  p <- newLink.calcPointFromProjection(ProjectionOverElement(lengthAvance, 0d))
                } yield {
                  val g = new GeoNode(p)
                  val newOut = PointUnitaryVector(g, t)
                  (newOut, pointstoMantain.map(_._2), new GeoLinkGraph(in, newOut) :: finalList)
                }).getOrElse((out, Nil, newLink :: finalList))

              }catch{
                case _: Throwable => (out, Nil, newLink :: finalList)
              }


            }else{
              (in,points,finalList)
            }
          case None => {
            if(points.length < 20)
              (in,points,finalList)
            else
              (currentLinkDraft.out,Nil,new GeoLinkGraph(in,currentLinkDraft.out) :: finalList)
          }
        }

      }
    }
    println("Links made")
    println(linksFinal.length)
    println(pointsNotUsed)

    //println(links.map(_.pointsDataCovering.size).mkString("\n"))

    val linkImproved = linksFinal.reverse
    linkImproved.zip(linkImproved.tail).foreach{case (a,b) =>
      a.next = Some(b)
      b.prev = Some(a)
    }

    Right(linkImproved.head)

    /*
    new EjeFromLinks(headLink).toEje.map{ eje =>
      val relevamientos = files.map(f => {
        val rel: RelevamientoIRI[IRIElementData] = RelevamientoIRI(f, cd => IRIElementData(cd))
        new RelevamientoIRIProgresivas(f.getAbsolutePath.trim,rel,eje,Progresiva(Integer.MIN_VALUE),Progresiva(Integer.MAX_VALUE))
      })

      val progPoints: Seq[(Int, Point)] = relevamientos.flatMap(_.elements).sortBy(_.progresiva).flatMap(i => {
        i.iriElementData.point.map(p => (i.progresiva,p.value))
      })

      val pstart = PointUnitaryVector(headLink.in.point + (headLink.in.direction*(-20)),headLink.in.direction)






    }


     */
  }
  def calcError(ejeElements: Seq[TEjeElement], points: Seq[TPoint]): Option[Double] = {
    val f: TPoint => Option[ElementPoint] =  EfficientSeqEjeElements.bruteForceCalculation(ejeElements,_)

    val q = points.map(f)
    val n = points.length
    val length = ejeElements.map(_.length).sum
    if(q.forall(_.isDefined)){
      val d = q.flatten.map(_.lengthToPointAbs).map(x => x*x).sorted
      val e = n match {
       case i if i<8 => d.sum
       case _ => d.sum
      }

      Some(e/(length*n))



    }else{
      None
    }

  }
}
