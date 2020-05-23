package io.vmchura.vevial.PlanarGeometric.ProgresiveEje

import com.typesafe.scalalogging.Logger
import io.vmchura.vevial.PlanarGeometric.BasicEje.TEfficientSeqEjeElements
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import io.vmchura.vevial.PlanarGeometric.EjeElement.{ElementPoint, TEjeElement}
import io.vmchura.vevial.PlanarGeometric.RestrictiveEje.{CircleSegmentRestrictions, EjeEfficientWithRestrictions, FaintSegmentRestrictions, RectSegmentRestrictions, Restriction, WithRestrictionsIncremental}

import scala.annotation.tailrec

case class EfficientEjeProgresiva(elements: List[WithProgresive])
  extends TEfficientSeqEjeElementsProgresiva {
  require(elements.nonEmpty)

  override def calcProgresive(ep: ElementPoint): Double = {
    val ewf = elements.find(_ == ep.ejeElementOwner)
    assert(ewf.isDefined)
    ewf.get.calcProgresive(ep)
  }

  override  def findPointByLength(lengthParam: Double): Option[ElementPoint] = {
    if(lengthParam <0 || lengthParam > length){
      None
    }else{

      val (wpOpt,lengthBeforeProg) = elements.foldLeft((None: Option[WithProgresive],0d: Double)){case ((prevOpt, pastLength),current) =>
        prevOpt match {
          case Some(value) => (Some(value),pastLength)
          case None =>
            if(pastLength + current.length >= lengthParam)
              (Some(current),pastLength)
            else (None,pastLength+current.length)
        }
      }
      wpOpt.flatMap{ wp =>
        wp.findPointByLength(lengthParam-lengthBeforeProg)
      }

    }
  }

  override def findPointByProgresive(progresive: Int): Option[TPoint] = {
    val eOpt = elements.find(wp =>wp.minProg <= progresive && progresive <= wp.maxProg)
    eOpt.flatMap{ e =>
      e.findPointByProgresive(progresive)
    }
  }

  override def slice(progIni: Int, progFin: Int): EfficientEjeProgresiva = {
    val filterElements = elements.filter( e => progIni <= e.minProg && e.maxProg <= progFin)
    if(filterElements.isEmpty)
      throw new IllegalArgumentException()
    EfficientEjeProgresiva(filterElements)
  }
}

object EfficientEjeProgresiva{

  private val logger = Logger(classOf[EfficientEjeProgresiva])
  def apply(elements: List[WithProgresive]): EfficientEjeProgresiva = new EfficientEjeProgresiva(elements)

  def apply[U <: TEfficientSeqEjeElements,V <: TEfficientSeqEjeElements](ejeEficient: EjeEfficientWithRestrictions[U,V]): EfficientEjeProgresiva = {
    trait MyEvent {
      val distanceFromOrigin: Double
      def <(o: MyEvent): Boolean = distanceFromOrigin < o.distanceFromOrigin
      def <=(o: MyEvent): Boolean = distanceFromOrigin <= o.distanceFromOrigin
      def >(o: MyEvent): Boolean = distanceFromOrigin > o.distanceFromOrigin
      def >=(o: MyEvent): Boolean = distanceFromOrigin >= o.distanceFromOrigin
      def || (o: MyEvent): Double = Math.abs(distanceFromOrigin-o.distanceFromOrigin)
      def ||? (o: MyEvent): Boolean = (this || o) < 1e-3
    }
    trait MyEventByRestriction extends MyEvent{
      def progresive: Double
    }
    trait MyEventByElement[A <: TEjeElement,B <: TEjeElement] extends MyEvent {
      def element: WithRestrictionsIncremental[A,B]
      def point: TPoint
      lazy val elementPoint: ElementPoint = ElementPoint(point,None,element)
    }
    case class RestrictionEvent(restriction: Restriction) extends MyEventByRestriction {
      override val distanceFromOrigin: Double = ejeEficient.lengthToPoint(restriction.elementPoint)

      override val progresive: Double = restriction.progresive
    }
    case class EndElement[A <: TEjeElement,B <: TEjeElement](element: WithRestrictionsIncremental[A,B]) extends MyEventByElement[A,B] {
      override  val distanceFromOrigin: Double = {
        val ep = ejeEficient.projectPoint(element.out.point)
        assert(ep.isDefined)
        ejeEficient.lengthToPoint(ep.get)
      }

      override val point: TPoint = element.out.point
    }
    case class BeginElement[A <: TEjeElement,B <: TEjeElement](element: WithRestrictionsIncremental[A,B]) extends MyEventByElement[A,B] {
      override  val distanceFromOrigin: Double = {
        val ep = ejeEficient.projectPoint(element.in.point)
        assert(ep.isDefined)
        ejeEficient.lengthToPoint(ep.get)
      }

      override val point: TPoint = element.in.point
    }
    case class MinusInfinityEvent() extends MyEventByRestriction {
      override val distanceFromOrigin: Double = Double.NegativeInfinity

      override val progresive: Double = Double.NegativeInfinity
    }
    case class PlusInfinityEvent() extends MyEventByRestriction {
      override val distanceFromOrigin: Double = Double.PositiveInfinity

      override val progresive: Double = Double.PositiveInfinity
    }

    val listRestrictionEvents: List[MyEventByRestriction] =

      {MinusInfinityEvent() :: PlusInfinityEvent() :: (if(ejeEficient.restrictions.isEmpty){
        val he = ejeEficient.element.elements.head
        val epIn = ElementPoint(he.in.point,None,he)
        RestrictionEvent(Restriction(epIn,0)) :: Nil
      }else{
          ejeEficient.restrictions.map(r => RestrictionEvent(r))
      })}.sortBy(_.distanceFromOrigin)



    /**
      * the objective is to add restrictions to both ends of elements
      *
      * state: le: ListElementEvents, still to process
      *        lr: ListRestrictions, toCalcEndPointsRestrictions
      *        lw: Array OfWithRestrictionsIncremental, result, since it is an array, can be updated
      */

    //add to a MyEventByElement and index
    case class MyEventByElementWithIndex(ee: MyEventByElement[TEjeElement,TEjeElement], index: Int)
    val lw: Array[WithRestrictionsIncremental[TEjeElement, TEjeElement]] = ejeEficient.elementsWithRestrictions

    val elementEvents = lw.zipWithIndex.toList.flatMap{case (e,i) =>Seq(MyEventByElementWithIndex(BeginElement(e),i),MyEventByElementWithIndex(EndElement(e),i))}
    val listElementsEvents: List[MyEventByElementWithIndex] =  elementEvents.sortBy(_.ee.distanceFromOrigin)









    def updateArray(elementByEvent: MyEventByElementWithIndex, progresiva: Double): Unit = {

      val indx = elementByEvent.index
      lw.update(indx,lw(indx).addRestriction(Restriction(elementByEvent.ee.elementPoint,progresiva)) match {
        case Left(value) =>
          logger.debug(s"can't add ${elementByEvent.ee.elementPoint}, $progresiva to $value")
          value

        case Right(value) => value

      })
    }


    @tailrec
    def addBothEndsRestrictions(le: List[MyEventByElementWithIndex],
                                lr: List[MyEventByRestriction]): Unit = {

      le match {
        case Nil => ()
        case MyEventByElementWithIndex(e,indx) :: _ =>


          lr match {
            case Nil => ()
            case _ :: Nil => ()
            case ri :: _  if e < ri => addBothEndsRestrictions(le,lr.tail)
            case _ :: rj :: _  if e > rj => addBothEndsRestrictions(le,lr.tail)
            case MinusInfinityEvent() :: rj :: _ =>
              updateArray(MyEventByElementWithIndex(e,indx),rj.progresive - (rj || e))
              addBothEndsRestrictions(le.tail,lr)
            case ri :: PlusInfinityEvent() :: _ =>
              updateArray(MyEventByElementWithIndex(e,indx),ri.progresive + (ri || e))
              addBothEndsRestrictions(le.tail,lr)
            case ri :: rj :: _ =>
              val deltaProgresiva = ri.progresive-rj.progresive
              val deltaDistance = ri.distanceFromOrigin-rj.distanceFromOrigin

              val newProgresiva = (deltaProgresiva,deltaDistance) match  {
                case (dp,_) if Math.abs(dp)<1e-3 => ri.progresive
                case (_,dd) if Math.abs(dd)<1e-3 => ri.progresive
                case (dp,dd) =>
                  ri.progresive-(ri.distanceFromOrigin-e.distanceFromOrigin)*dp/dd
              }
              updateArray(MyEventByElementWithIndex(e,indx),newProgresiva)

              addBothEndsRestrictions(le.tail,lr)
          }
      }
    }

    addBothEndsRestrictions(listElementsEvents,listRestrictionEvents)

    import WithDistributionFormula._
    val lprog: List[WithProgresive] = lw.map {
      case r: RectSegmentRestrictions => WithDistributionFormula.convert(r)
      case c: CircleSegmentRestrictions => WithDistributionFormula.convert(c)
      case f: FaintSegmentRestrictions =>  WithDistributionFormula.convert(f)
      case _ => throw new IllegalArgumentException("it is not RectSegmentRestricctions neither Circ or Faint")
    }.toList

      new EfficientEjeProgresiva(lprog)

  }
}