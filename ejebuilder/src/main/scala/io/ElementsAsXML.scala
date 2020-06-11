package io

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TDirection, TPoint}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TDirection.{AnyDirection, Direction}
import io.vmchura.vevial.elementdata.UncertainData

import scala.xml.{Elem, Node, NodeSeq}

object ElementsAsXML {

  def savePointAsXML(tpoint: TPoint): Elem = {
    <point>
      <x>{f"${tpoint.x}%.8f"}</x>
      <y>{f"${tpoint.y}%.8f"}</y>
    </point>
  }

  def loadPoint(node: Node): TPoint = {
    (for{
      x <-  (node \\ "x").headOption
      y <-  (node \\ "y").headOption
      xv <- x.text.toDoubleOption
      yv <- y.text.toDoubleOption
    }yield{
      Point(xv,yv)
    }).getOrElse(throw new IllegalArgumentException("node doesnt not represent a point"))
  }

  def saveDirectionAsXML(tdirection: TDirection): Elem = {
    <direction isDefined={s"${tdirection.isInstanceOf[Direction]}"}>
      {
      tdirection match {
        case _: AnyDirection => NodeSeq.Empty
        case d: Direction =>
          <dx>{f"${d.dx}%.8f"}</dx>
            <dy>{f"${d.dy}%.8f"}</dy>
      }
      }

    </direction>
  }

  def loadDirection(node: Node): TDirection = {

    if((node \@ "isDefined").equals("true")){
      (for{
        dx <-  (node \\ "dx").headOption
        dy <-  (node \\ "dy").headOption
        dxv <- dx.text.toDoubleOption
        dyv <- dy.text.toDoubleOption
      }yield{
        TDirection(dxv,dyv)
      }).getOrElse(throw new IllegalArgumentException("node doesnt not represent a point"))
    }else{
      TDirection()
    }

  }

  def loadPointUnitaryVector(node: Node): PointUnitaryVector = {
    (for{
      pointNode <- (node \\ "point").headOption
      directionNode <- (node \\ "direction").headOption

    }yield{
      val point = loadPoint(pointNode)
      val direction = loadDirection(directionNode)
      PointUnitaryVector(point,direction)
    }).getOrElse(throw new IllegalArgumentException("node doesnt not represent a point vector"))

  }

  def saveUncertainElement[E,B](ue: UncertainData[E,B], tagBlock: String, saveElement: E => Elem): Elem = {
    <UElement sigma2={s"${ue.sigma2}"} tag={tagBlock}>
      {saveElement(ue.value)}
    </UElement>
  }
  def loadUncertainElement[E,B](node: Node, tagBlock: String, loadElement: Node => Option[E], buildUncertainElement: (Double,E) => B): Option[B] = {
    for{
      value <- (node \ tagBlock).headOption
      sigma2Node <- (node \ "@sigma2").headOption
      element <- loadElement(value)
      sigma2 <- sigma2Node.text.toDoubleOption
    }yield{
      buildUncertainElement(sigma2,element)
    }

  }
}
