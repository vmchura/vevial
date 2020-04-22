package AutomaticBuilder.models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint

import scala.collection.mutable.ListBuffer

trait TObserver {
  def elementObserved: TElementCanImprove
  private val current_elementsAdded = ListBuffer.empty[TProjection]
  final def elementsAdded(): List[TProjection] = current_elementsAdded.toList
  final def addProjection(p: TPoint): Unit = elementObserved.calcProjection(p).foreach(current_elementsAdded.append)


}
