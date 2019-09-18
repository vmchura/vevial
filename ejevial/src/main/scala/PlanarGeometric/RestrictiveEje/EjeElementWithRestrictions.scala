package PlanarGeometric.RestrictiveEje

import PlanarGeometric.BasicEje.{EfficientSeqEjeElements, TEfficientSeqEjeElements}
import PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector}
import PlanarGeometric.EjeElement._


case class Restriction(elementPoint: ElementPoint, progresive: Double)

trait WithRestrictionsIncremental[+A<: TEjeElement,+B<: TEjeElement] extends TEjeElementByVariable[B] {
  type WTT = WithRestrictionsIncremental[TEjeElement,TEjeElement]
  def restrictions: List[Restriction]
  val element: B
  def addRestriction(newRestriction : Restriction): Either[WTT,WTT]
  def addRestriction(progresivePoint: ProgresivePoint): Either[WTT,WTT] = {
    element.projectPoint(progresivePoint).map{ e =>
        addRestriction(Restriction(e,progresivePoint.progresive))
    }.getOrElse(Left(this.asInstanceOf[WTT]))

  }



}
trait WithRestrictionIncrementalEje[+ A<: TEfficientSeqEjeElements, +B <: TEfficientSeqEjeElements] extends TEjeSequenceElementByVariable[B] with TEfficientSeqEjeElements { this: A =>
  def restrictions: List[Restriction]
  val element: B
  def addRestriction(newRestriction : Restriction): Either[A,A]
  def addRestriction(progresivePoint: ProgresivePoint): Either[A,A] = {
    element.projectPoint(progresivePoint).map{ e =>
      addRestriction(Restriction(e,progresivePoint.progresive))
    }.getOrElse(Left(this))

  }
}

trait TEjeSequenceElementByVariable[+B <: TEfficientSeqEjeElements] extends TEfficientSeqEjeElements {
  val element: B

  override val in: PointUnitaryVector = element.in

  override val length: Double = element.length

  override def ==?(o: TEjeElement): Boolean = element.==?(o)

  override def lengthToPoint(point: ElementPoint): Double = element.lengthToPoint(point)

  override val out: PointUnitaryVector = element.out

  override def pointIsInsideElement(point: Point): Boolean = element.pointIsInsideElement(point)

  override def projectPoint(point: Point): Option[ElementPoint] = element.projectPoint(point)

  override def elements: List[TEjeElement] = element.elements
}
trait TEjeElementByVariable[+B <: TEjeElement] extends TEjeElement {
  def element: B

  override def in: PointUnitaryVector = element.in

  override def length: Double = element.length

  override def ==?(o: TEjeElement): Boolean = element.==?(o)

  override def lengthToPoint(point: ElementPoint): Double = element.lengthToPoint(point)

  override def out: PointUnitaryVector = element.out

  override def pointIsInsideElement(point: Point): Boolean = element.pointIsInsideElement(point)

  override def projectPoint(point: Point): Option[ElementPoint] = element.projectPoint(point)
  def indexTag: Int

  override def toString: String = s"TEjeElementByVariable[$indexTag]"
}
object TEjeElementByVariable{
  private var providerTag = LazyList.from(1)
  def getNextTag(): Int = {

    val result = providerTag.head
    providerTag = providerTag.tail
    result
  }
}

trait SimpleElementWithRestrictions[ +A<: TEjeElement  ,+B<: TSimpleEjeElement] extends WithRestrictionsIncremental[A,B]

case class RectSegmentRestrictions(element: RectSegment,restrictions: List[Restriction])
    extends SimpleElementWithRestrictions[RectSegmentRestrictions,RectSegment]  {
    override val indexTag = TEjeElementByVariable.getNextTag()

  override def equals(o: Any) = o match {
    case that: RectSegmentRestrictions => that.element == element
    case _ => false
  }
  override def hashCode = element.hashCode()

  override def addRestriction(newRestriction: Restriction): Either[WTT,WTT] =
    if(newRestriction.elementPoint.ejeElementOwner == element || newRestriction.elementPoint.ejeElementOwner == this)
      Right(copy(restrictions = newRestriction :: restrictions))
    else
      Left(this)


}

case class CircleSegmentRestrictions(element: CircleSegment, restrictions: List[Restriction])
  extends SimpleElementWithRestrictions[CircleSegmentRestrictions,CircleSegment] {

  override val indexTag = TEjeElementByVariable.getNextTag()
  override def equals(o: Any) = o match {
    case that: CircleSegmentRestrictions => that.element == element
    case _ => false
  }
  override def hashCode = element.hashCode()
  override def addRestriction(newRestriction: Restriction): Either[WTT,WTT] =
    if(newRestriction.elementPoint.ejeElementOwner == element || newRestriction.elementPoint.ejeElementOwner == this)
      Right(copy(restrictions = newRestriction :: restrictions))
    else
      Left(this)

}

case class FaintSegmentRestrictions(element: FaintElement, restrictions: List[Restriction])
  extends SimpleElementWithRestrictions[FaintSegmentRestrictions,FaintElement] {
  //override  type WTT = FaintSegmentRestrictions
//  override type WTT = SimpleElementWithRestrictions[TEjeElement, TEjeElement]
override def equals(o: Any) = o match {
  case that: FaintSegmentRestrictions => that.element == element
  case _ => false
}
  override def hashCode = element.hashCode()
  /*
  override def addRestriction(newRestriction: Restriction): Either[FaintSegmentRestrictions,FaintSegmentRestrictions] =
    if(newRestriction.elementPoint.ejeElementOwner == element)
      Right(copy(restrictions = newRestriction :: restrictions))
    else
      Left(this)

   */

  override val indexTag = TEjeElementByVariable.getNextTag()

  override def addRestriction(newRestriction: Restriction): Either[WTT, WTT] = {
    if(newRestriction.elementPoint.ejeElementOwner == element || newRestriction.elementPoint.ejeElementOwner == this)
      Right(copy(restrictions = newRestriction :: restrictions))
    else
      Left(this)
  }

}

case class EjeEfficientWithRestrictions[+U <: TEfficientSeqEjeElements, V <: TEfficientSeqEjeElements](element: EfficientSeqEjeElements,
                                                                             restrictions: List[Restriction],
                                                                             elementsWithRestrictions: Array[WithRestrictionsIncremental[TEjeElement,TEjeElement]])
  extends WithRestrictionIncrementalEje[EjeEfficientWithRestrictions[U,V],EfficientSeqEjeElements] {

  private val element2Index: Map[TEjeElement, Int] = element.elements.map{ e =>
      e -> {
        val v = elementsWithRestrictions.find(_.element == e)
        assert(v.isDefined)
        elementsWithRestrictions.indexOf(v.get)
      }

  }.toMap


  override def addRestriction(newRestriction: Restriction): Either[EjeEfficientWithRestrictions[U, V], EjeEfficientWithRestrictions[Nothing, V]] = {
    val indexOption = element2Index.get(newRestriction.elementPoint.ejeElementOwner)
    assert(indexOption.isDefined)
    val wr = elementsWithRestrictions(indexOption.get)
    val wrNew = wr.addRestriction(newRestriction)

    wrNew match {
      case Left(_) => Left(this)
      case Right(value) =>

        //val v : WithRestrictionsIncremental[TEjeElement,TEjeElement] = value

        val newElementsWithRestrictions: Array[WithRestrictionsIncremental[TEjeElement,TEjeElement]] = elementsWithRestrictions.updated(indexOption.get,value)

        Right(EjeEfficientWithRestrictions(element,newRestriction :: restrictions,newElementsWithRestrictions))
    }

  }


}

object EjeEfficientWithRestrictions {
  def apply(element: EfficientSeqEjeElements): EjeEfficientWithRestrictions[TEfficientSeqEjeElements,TEfficientSeqEjeElements] = {

    val elementsWithRestrictions: Array[WithRestrictionsIncremental[TEjeElement,TEjeElement]] = element.elements.map{
        case r: RectSegment => RectSegmentRestrictions(r, Nil)
        case c: CircleSegment => CircleSegmentRestrictions(c, Nil)
        case f: FaintElement => FaintSegmentRestrictions(f, Nil)
      }.toArray



    new EjeEfficientWithRestrictions(element,Nil,elementsWithRestrictions)

  }
}


