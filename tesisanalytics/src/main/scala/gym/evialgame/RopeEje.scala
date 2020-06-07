package gym.evialgame

import gym.evialgame.RopeEje.RopeElement
import io.vmchura.vevial.PlanarGeometric.BasicEje.EfficientSeqEjeElements
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TPoint}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{CircleSegment, ElementPoint, RectSegment, TEjeElement}
case class RopeEje(nodes: Seq[RopeNode]) {
  def turnRopeNode(indx: Int, initialTorque: Double): RopeEje = {

    val initialForce = 100.0
    def calcForce(x: Int): Double = initialForce*Math.pow(1d/(x+1d),1.1d)
    if(indx >= nodes.length)
      throw  new IllegalArgumentException("turning a node which is not defined")
    case class TurnState(prevNode: Option[RopeNode],prevUpdatedNode: Option[RopeNode], torque: Double)
    val (preflix,suffix) = nodes.splitAt(indx)
    val suffixUpdated = suffix.zipWithIndex.scanLeft(TurnState(None,None,initialTorque)){case (prevTurnState, (currentNode,indx)) =>

      prevTurnState match {
        /**
          * add beta to the prevNode, y aproximar con currentNode
          */
        case TurnState(Some(prevNode),Some(prevNodeUpdated), torque) =>
          val nextPoint = prevNode.turnNode(prevNodeUpdated.originalPoint,torque,calcForce(indx),currentNode.originalPoint)
          TurnState(Some(currentNode),Some(currentNode.copy(originalPoint = nextPoint)),torque*0.6)
        case TurnState(_,_, torque) => TurnState(Some(currentNode),Some(currentNode),torque)
      }
    }.flatMap(_.prevUpdatedNode)

    RopeEje(preflix ++ suffixUpdated)

  }
  lazy val elements: Seq[TEjeElement] = { nodes.zip(nodes.tail).map{ case (a,b) => RopeElement(a.originalPoint,b.originalPoint)} }

  override def toString: String = nodes.take(10).mkString("-")


  def projectPoint(point: Point, debug: Boolean = false): Option[ElementPoint] = EfficientSeqEjeElements.bruteForceCalculation(elements,point,debug)

  def calcProgresiva(element: TEjeElement): Int = {
    val indx = elements.indexWhere(_ == element)
    if(indx >= 0){
      indx*RopeEje.distanceBetweenNodes.toInt
    }else{
      throw  new IllegalArgumentException("Element not present")
    }
  }
}

object RopeEje{
  val numElements = 200
  val distanceBetweenNodes = 1.0
  val wDisccount = 0.6
  def apply(): RopeEje = new RopeEje((0 to numElements).map(i => RopeNode(Point(i*distanceBetweenNodes,0))))
  private val rigidez: Double = 0.9

  case class RopeElement(originPoint: Point, endPoint: Point) extends TEjeElement {
    private val pVector = endPoint-originPoint
    override val length: Double = !pVector
    override val in: PointUnitaryVector = PointUnitaryVector(originPoint,pVector.direction)
    override val out: PointUnitaryVector = PointUnitaryVector(endPoint,pVector.direction)

    override def projectPoint(point: TPoint): Option[ElementPoint] = {
      val v = point - originPoint
      val s = in.direction
      val distance  =  (v * s) / (s * s)
      val projection = originPoint + s * distance

      if (pointIsInsideElement(projection))
        Some(ElementPoint(projection,point-?projection,this))
      else
        None

    }

    override def pointIsInsideElement(point: TPoint): Boolean = {
      val sumLengths = (point-?originPoint).map(_.magnitude).getOrElse(0d) + (point-?endPoint).map(_.magnitude).getOrElse(0d)
      length*2 > sumLengths
    }

    override def lengthToPoint(point: ElementPoint): Double = throw new IllegalArgumentException()

    override def leftmostPoint: Point = throw new IllegalArgumentException()

    override def rightmostPoint: Point = throw new IllegalArgumentException()

    override def upperPoint: Point = throw new IllegalArgumentException()

    override def lowerPoint: Point = throw new IllegalArgumentException()

  }

}
