package algorithms

import io.vmchura.vevial.elementdata.{UDirection, UPoint}
import models.{PointTangentDefined, TPointTangent}

import scala.xml.{Elem, Node}

case class ProgPointTangent(prog: Int, pointTangent: TPointTangent)

object ProgPointTangent{
  import io.ElementsAsXML._
  def saveAsXML(p: ProgPointTangent): Elem = {
    <ProgPointTangent prog={s"${p.prog}"} defined={s"${p.pointTangent.point.isDefined}"}>
      {p.pointTangent.point.fold(<UndefinedPoint></UndefinedPoint>)(point => saveUncertainElement(point,"point",savePointAsXML))}
      {saveUncertainElement(p.pointTangent.tangent,"direction",saveDirectionAsXML)}
    </ProgPointTangent>
  }
  def loadFromXML(root: Node): Option[ProgPointTangent] = {
    for{
      progNode <- (root \ "@prog").headOption
      prog <- progNode.text.toIntOption
      definedNode <- (root \ "@defined").headOption
      _ <- if(definedNode.text.equals("true")) Some(true) else None
      (pointNode,directionNode) <- {
        val uElement = root \ "UElement"
        for{
          p <- uElement.find(_.attribute("tag").map(_.text).contains("point"))
          d <- uElement.find(_.attribute("tag").map(_.text).contains("direction"))
        }yield{
          (p,d)
        }
      }
      point <- loadUncertainElement(pointNode,"point",n => try Some(loadPoint(n)) catch {
        case _ : Throwable => None
      },(sigma,element) => UPoint(element,sigma))
      direction <- loadUncertainElement(directionNode,"direction", n => try Some(loadDirection(n)) catch {
        case _ : Throwable => None
      },(sigma,element) => UDirection(element,sigma))
    }yield{
      ProgPointTangent(prog,PointTangentDefined(Some(point), direction))
    }
  }
}