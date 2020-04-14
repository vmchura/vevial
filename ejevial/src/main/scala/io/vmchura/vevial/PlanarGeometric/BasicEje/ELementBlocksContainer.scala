package io.vmchura.vevial.PlanarGeometric.BasicEje

import io.vmchura.vevial.PlanarGeometric.EjeElement.{ElementPoint, TEjeElement}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import PointsOnElementGenerator._

class ELementBlocksContainer(ini: Double, end: Double, binWidth: Double,  getter: ElementPoint => Double,sideGap: Int = 2) {
  val offset = (ini/binWidth).toInt-sideGap
  private def findIndx(x: Double): Int = (x/binWidth).toInt-offset
  val n = findIndx(end)+sideGap+1
  val blocks: Array[ListBuffer[TEjeElement]] = (0 to n).map(_ => mutable.ListBuffer.empty[TEjeElement]).toArray

  def addSameElementPoint(projections: Seq[ElementPoint]): Unit = {
    if(projections.nonEmpty) {
      val indx = projections.map(ep => findIndx(getter(ep))).distinct
      val element = projections.head.ejeElementOwner
      indx.foreach(i => blocks(i).append(element))
    }
  }

  def removeSameElementPoint(element: TEjeElement): Unit = {
    val ep = PointsOnElementGenerator.generatePoints(element)
    val indx = ep.map(ep => findIndx(getter(ep))).distinct
    indx.foreach(i => {
      val j = blocks(i).indexWhere(_ == element)
      if(j>=0)
        blocks(i).remove(j)
    })

  }

  def getElementsAround(x: Double): Set[TEjeElement] = {
    val i = Math.max(0,findIndx(x)-1)
    val j = Math.min(n-1,findIndx(x)+1)
    (i to j).flatMap(k => blocks(k)).toSet
  }

}
