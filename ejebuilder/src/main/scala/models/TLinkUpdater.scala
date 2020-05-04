package models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import io.vmchura.vevial.PlanarGeometric.EjeElement.TEjeElement


trait TLinkUpdater{
  def removeElements: Seq[TEjeElement] => Unit
  def addElements: Seq[TEjeElement] => Unit
  def removeLinks: Seq[TLinkPoint] => Unit
  def addLinks: Seq[TLinkPoint] => Unit
  final def updateSegment[A <: TPoint](oldLinkBegin: TLinkPoint, oldLinkEnd: TLinkPoint,
                                       newLinkBegin: TLinkPoint, newLinkEnd: TLinkPoint): Unit = {
    val linksToDrop = oldLinkBegin.untilTarget(oldLinkEnd)
    val elementsToDrop = linksToDrop.flatMap(_.elements)

    removeLinks(linksToDrop)
    removeElements(elementsToDrop)

    val linksToAdd = newLinkBegin.untilTarget(newLinkEnd)
    val elementsToAdd = linksToAdd.flatMap(_.elements)

    addElements(elementsToAdd)
    addLinks(linksToAdd)

    oldLinkBegin.prev match {
      case Some(prev) =>
        prev.next = Some(newLinkBegin)
        newLinkBegin.prev = Some(prev)
      case None =>
        newLinkBegin.prev = None
    }

    oldLinkEnd.next match {
      case Some(next) =>
        next.prev = Some(newLinkEnd)
        newLinkEnd.next = Some(next)
      case None =>
        newLinkEnd.next = None
    }


  }
  final def dropSegment[A <: TPoint](oldLinkBegin: TLinkPoint, oldLinkEnd: TLinkPoint): Unit = {
    val linksToDrop = oldLinkBegin.untilTarget(oldLinkEnd)
    val elementsToDrop = linksToDrop.flatMap(_.elements)

    removeLinks(linksToDrop)
    removeElements(elementsToDrop)

    oldLinkBegin.prev.foreach{ prev =>
      prev.next = oldLinkEnd.next
    }

    oldLinkEnd.next.foreach{ next =>
      next.prev = oldLinkBegin.prev

    }

  }
}

