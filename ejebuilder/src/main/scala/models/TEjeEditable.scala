package models

import AutomaticBuilder.models.{ElementActionToImprove, NoAction, SetPointAt, SimpleAgentEjeEvaluator, TElementCanImprove, TIterativeImproving}
import algorithms.LinearEquationsSolver
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TPoint}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{ElementPoint, TEjeElement}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait TEjeEditable extends TLinkManager with TLinkUpdater  with TIterativeImproving{
  protected val logger: Logger = LoggerFactory.getLogger(this.getClass)

  private val currentGeoNodes: mutable.Set[GeoNode] = mutable.Set.empty[GeoNode]
  def geoNodesPresent: List[GeoNode] = currentGeoNodes.toList

  protected def geoNodeAdded(geoNode: GeoNode): Unit
  final def addGeoNode(geoNode: GeoNode): Unit = {
    currentGeoNodes += geoNode
    geoNodeAdded(geoNode)
  }
  protected def geoNodeRemoved(geoNode: GeoNode): Unit
  final def removeGeoNode(geoNode: GeoNode): Unit = {
    currentGeoNodes -= geoNode
    geoNodeRemoved(geoNode)
  }
  def elementAdded(e: TEjeElement): Unit
  def elementRemoved(e: TEjeElement): Unit
  private val mutableEje = new MutableEje(initialEjeElements)
  logger.info(s"Num elements on mutable eje: ${mutableEje.elements.length}")

  val linksEnabled = scala.collection.mutable.Set.empty[TElementCanImprove]

  private val pointsDataFree = ListBuffer.empty[TPoint]
  final def setInitialPointsFree(freePoints: IterableOnce[TPoint]): Unit = {
    pointsDataFree ++= freePoints
    addLinks(initialLinks)
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


  def elementByPosition(point: TPoint): Option[Either[GeoNode,ElementPoint]] = {
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
        addGeoNode(newGeoNode)
        val leftNth = left.prevNth(5)
        val rightNth = right.nextNth(5)
        val nodes = leftNth.nodesUntilTarget(rightNth).map(_.asInstanceOf[GeoNode])
        val link = TLinkManager.buildLink(nodes.toArray,Some(leftNth.in.direction),Some(rightNth.out.direction))
        link.foreach(link => {
          addGeoNodeLinkRelation(link.in.point,link)
          addGeoNodeLinkRelation(link.out.point,link)
        })

        updateSegment(leftNth,rightNth,link.head,link.last)

        newGeoNode
      case _ => throw new IllegalArgumentException("NOT A GEO LINK?")
    }


  }
  def dropNode(geoNode: GeoNode): Unit = {
    logger.debug(s"Dropping node: $geoNode")
    geoNodeLinkMap.get(geoNode) match {
      case Some(section) =>
        val in = section.in.point
        val out = section.out.point
        val prev = section.prev
        val next = section.next
        val (xOpt, yOpt) = if (in == geoNode) {
          (prev, Some(section))
        } else {
          if (out == geoNode) {
            (Some(section), next)
          } else {
            throw new IllegalArgumentException("Is not a correct END POINT")
          }
        }

        removeGeoNode(geoNode)

        (xOpt,yOpt) match {
          case (Some(x),Some(y)) =>
            val ln = new GeoLinkGraph(x.in,y.out)
            (x.in.point,y.out.point) match {
              case (a: GeoNode, b: GeoNode) =>
                addGeoNodeLinkRelation(a,ln)
                addGeoNodeLinkRelation(b,ln)
              case _ => throw new IllegalStateException("not geoNode")
            }

            updateSegment(x,y,ln,ln)

          case (Some(x),None) =>

            dropSegment(x,x)


          case (None,Some(y)) =>

            dropSegment(y,y)
          case _ => throw new IllegalArgumentException("Nor prev nor next")
        }



      case None => throw  new IllegalStateException("GEO NODE NOT REGISTERED")
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

        val link = TLinkManager.buildLink(nodes.toArray,Some(leftNth.in.direction),Some(rightNth.out.direction))

        link.foreach(link => {
          addGeoNodeLinkRelation(link.in.point,link)
          addGeoNodeLinkRelation(link.out.point,link)
        })

        updateSegment(leftNth,rightNth,link.head,link.last)

      case None => throw  new IllegalStateException("GEO NODE NOT REGISTERED")

    }

    addGeoNode(newGeoNode)
    removeGeoNode(geoNode)


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



  def headLink(): Option[TLinkPoint] = {
    geoNodeLinkMap.find{case (_,lg) => isElementValid(lg)}.map(_._2.firstDefined())
  }
}
