package models

import io.vmchura.vevial.PlanarGeometric.BasicEje.{ELementBlocksContainer, EfficientSeqEjeElements, PointsOnElementGenerator, TSeqEjeElementsBase}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{ElementPoint, TEjeElement, TSimpleEjeElement}
import PointsOnElementGenerator._
import io.vmchura.vevial.PlanarGeometric.ConfigParametersGeometric
import io.vmchura.vevial.PlanarGeometric.ConfigParametersGeometric.distanceToFindProjection

class MutableEje(val elements: List[TEjeElement]) extends  TSeqEjeElementsBase {
  require(elements.nonEmpty)


  override val length: Double = elements.map(_.length).sum
  override val in: PointUnitaryVector = elements.head.in
  override val out: PointUnitaryVector = elements.last.out

  override def append(o: TSeqEjeElementsBase): TSeqEjeElementsBase = throw new NotImplementedError("cant append")



  override def pointIsInsideElement(point: Point): Boolean = throw new NotImplementedError()

  override def lengthToPoint(point: ElementPoint): Double = throw new NotImplementedError()

  override val leftmostPoint: Point = elements.map(_.leftmostPoint).minBy(_.x)

  override val  rightmostPoint: Point = elements.map(_.rightmostPoint).maxBy(_.x)

  override val  upperPoint: Point = elements.map(_.upperPoint).maxBy(_.y)

  override val  lowerPoint: Point = elements.map(_.lowerPoint).minBy(_.y)


  private val (blockX,blockY) = {
    val groupPoints = elements.map(e => PointsOnElementGenerator.generatePoints(e))
    val bx = new ELementBlocksContainer(leftmostPoint.x,rightmostPoint.x,distanceToFindProjection,_.x)
    val by = new ELementBlocksContainer(lowerPoint.y,upperPoint.y,distanceToFindProjection,_.y)

    groupPoints.foreach(bx.addSameElementPoint)
    groupPoints.foreach(by.addSameElementPoint)

    (bx,by)
  }

  override def projectPoint(point: Point): Option[ElementPoint] = {

    val elementsX = blockX.getElementsAround(point.x)
    val elementsY = blockY.getElementsAround(point.y)

    val intersection = elementsX intersect elementsY

    EfficientSeqEjeElements.bruteForceCalculation(intersection.toList,point,false)

  }

  def addElement(element: TEjeElement): Unit = {
    val points = PointsOnElementGenerator.generatePoints(element)
    blockX.addSameElementPoint(points)
    blockY.addSameElementPoint(points)
  }

  def removeElement(element: TEjeElement): Unit = {
    blockX.removeSameElementPoint(element)
    blockY.removeSameElementPoint(element)
  }

  def endPointsClosest(point: Point): Option[ElementPoint] = {
    val elementsX = blockX.getElementsAround(point.x)
    val elementsY = blockY.getElementsAround(point.y)

    val intersection = elementsX intersect elementsY
    intersection.toList.flatMap(x => {
      val inEP = ElementPoint(x.in.point,None,x)
      val outEP = ElementPoint(x.out.point,None,x)
      List(inEP,outEP)
    }).minByOption(s => !(s-point))
  }

}

