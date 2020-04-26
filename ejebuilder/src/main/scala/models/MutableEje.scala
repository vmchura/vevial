package models

import io.vmchura.vevial.PlanarGeometric.BasicEje.{ELementBlocksContainer, EfficientSeqEjeElements, PointsOnElementGenerator, TSeqEjeElementsBase}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TPoint}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{ElementPoint, TEjeElement, TSimpleEjeElement}
import PointsOnElementGenerator._
import io.vmchura.vevial.PlanarGeometric.ConfigParametersGeometric
import io.vmchura.vevial.PlanarGeometric.ConfigParametersGeometric.distanceToFindProjection

class MutableEje(val elements: List[TEjeElement]) extends  TSeqEjeElementsBase {
  require(elements.nonEmpty)
  println(s"Num elements: ${elements.length}")

  override val length: Double = elements.map(_.length).sum
  override val in: PointUnitaryVector = elements.head.in
  override val out: PointUnitaryVector = elements.last.out

  override def append(o: TSeqEjeElementsBase): TSeqEjeElementsBase = throw new NotImplementedError("cant append")



  override def pointIsInsideElement(point: TPoint): Boolean = throw new NotImplementedError()

  override def lengthToPoint(point: ElementPoint): Double = throw new NotImplementedError()

  override val leftmostPoint: TPoint = elements.map(_.leftmostPoint).minBy(_.x)

  override val  rightmostPoint: TPoint = elements.map(_.rightmostPoint).maxBy(_.x)

  override val  upperPoint: TPoint = elements.map(_.upperPoint).maxBy(_.y)

  override val  lowerPoint: TPoint = elements.map(_.lowerPoint).minBy(_.y)


  private val (blockX,blockY) = {
    val bx = new ELementBlocksContainer(leftmostPoint.x,rightmostPoint.x,12,_.x)
    val by = new ELementBlocksContainer(lowerPoint.y,upperPoint.y,12,_.y)



    (bx,by)
  }

  override def projectPoint(point: TPoint): Option[ElementPoint] = {

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

  def endPointsClosest(point: TPoint): Option[ElementPoint] = {
    val elementsX = blockX.getElementsAround(point.x)
    val elementsY = blockY.getElementsAround(point.y)

    val intersection = elementsX intersect elementsY
    intersection.toList.flatMap(x => {

      val inEP = ElementPoint(x.in.point,None,x)
      val outEP = ElementPoint(x.out.point,None,x)
      (if((!(x.in.point - point)) < 1){
        List(inEP)
      }else{
        Nil
      }) ++ (if((!(x.out.point - point)) < 1){
        List(outEP)
      }else{
        Nil
      }) ++ Nil

    }).minByOption(s => !(s-point))
  }

}

