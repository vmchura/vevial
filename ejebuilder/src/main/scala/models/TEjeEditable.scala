package models

import AutomaticBuilder.models.{ElementActionToImprove, NoAction, SetPointAt, SimpleAgentEjeEvaluator, TElementCanImprove, TIterativeImproving}
import algorithms.LinearEquationsSolver
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TPoint}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{ElementPoint, TEjeElement}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.ListBuffer

trait TEjeEditable extends TLinkManager with TLinkUpdater  with TIterativeImproving{
  protected val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def geoNodeAdded(geoNode: GeoNode): Unit
  def geoNodeRemoved(geoNode: GeoNode): Unit
  def elementAdded(e: TEjeElement): Unit
  def elementRemoved(e: TEjeElement): Unit
  protected val (elementsToObserve,linksToObserve) = initialElementsGenerated
  private val mutableEje = new MutableEje(elementsToObserve)
  logger.info(s"Num elements on mutable eje: ${mutableEje.elements.length}")

  val linksEnabled = scala.collection.mutable.Set.empty[TElementCanImprove]

  private val pointsDataFree = ListBuffer.empty[TPoint]
  final def setInitialPointsFree(freePoints: IterableOnce[TPoint]): Unit = {
    pointsDataFree ++= freePoints
    addLinks(linksToObserve)
  }


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
      case s: TEjeElementTemporal =>
        val section = s.ejeSection

        val newGeoNode = new GeoNode(elementPoint.sourcePoint)


        val (left,right) = createPairLinks(section.in,section.out,newGeoNode)


        updateSegment(section,section,left,right)
        val leftNth = left.prevNth(5)
        val rightNth = right.nextNth(5)
        val nodes = leftNth.nodesUntilTarget(rightNth).map(_.asInstanceOf[GeoNode])
        val link = buildLink(nodes.toArray,Some(leftNth.in.direction),Some(rightNth.out.direction))

        updateSegment(leftNth,rightNth,link.head,link.last)
        newGeoNode
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

  final override def addLinks: Seq[TLinkPoint] => Unit = links => {
    logger.info(s"Adding links ${links.length}")
    linksEnabled.addAll(links)
    assignPointsFree()
    links.foreach{ link =>
      val observer = new ObserverImpl(link)
      link.pointsDataCovering.foreach(observer.addProjection)
      val eat = ElementActionToImprove(link,SimpleAgentEjeEvaluator.deliberateAnAction(observer))
      addActionToImprove(eat)
    }
  }

  final override def removeLinks: Seq[TLinkPoint] => Unit = _.foreach(link => {
    linksEnabled.remove(link)
    pointsDataFree.appendAll(link.pointsDataCovering)
  })

  def assignPointsFree(): Unit = {
    logger.info(s"assinging ${pointsDataFree.length} Points Free")
    val pointsFree = pointsDataFree.filter(p => {
      mutableEje.projectPoint(p) match {
        case None =>
          true
        case Some(ep) =>
          ep.ejeElementOwner match {
            case temp: TEjeElementTemporal =>
              temp.ejeSection.addPointCovered(p)
              false
            case _ => true
          }
      }
    })

    pointsDataFree.clear()
    pointsDataFree ++= pointsFree
    logger.info(s"Points free after asigning: ${pointsDataFree.length}")
  }

  override final def applyUpgrade(ea: ElementActionToImprove): Boolean = {
    if(linksEnabled(ea.elementCanImprove)){
      ea.actionImproveEje match {
        case NoAction => ()
        case s: SetPointAt =>
          val e = ea.elementCanImprove
          e.calcPointFromProjection(s) match {
            case Some(np) =>
              elementByPosition(np) match {
                case Some(Left(_)) => println("Not changing nothing")
                case Some(Right(elementPoint)) => createInnerNode(elementPoint)
                case None => throw new IllegalArgumentException("action is recommended, projection not found")
              }
            case None => throw new IllegalStateException("Action is recommended, no point cant ve reconstructe found")

          }



      }

      true


    }else{
      false
    }
  }


  override final def isElementValid(element: TElementCanImprove): Boolean = linksEnabled.contains(element)



}
