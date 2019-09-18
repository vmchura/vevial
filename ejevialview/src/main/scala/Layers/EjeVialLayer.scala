package Layers

import PlanarGeometric.EjeElement.{TCircleSegment, TFaintElement, TRectSegment}
import PlanarGeometric.ProgresiveEje.{EfficientEjeProgresiva, WithProgresive}
import ShapeGenerator.EjeConverter
import scalafx.beans.property.DoubleProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Arc, ArcType, Line, Shape}

class EjeVialLayer(eje: EfficientEjeProgresiva) extends TLayer[WithProgresive] {




  /**
    * update nodes drawn, (x,y) top left corner (u,v) bottom right corner
    *
    * @param x
    * @param y
    * @param u
    * @param v
    */
  override def setListenerPanelUpdate(x: DoubleProperty, y: DoubleProperty, u: DoubleProperty, v: DoubleProperty): Unit = ()

  override def conversor(e: WithProgresive): Seq[Node] = EjeVialLayer.elementConversor(e)

  addAll(eje.elements)
}



object EjeVialLayer {





  private val drawableConvert2Nodes:  WithProgresive =>  Seq[Node] = {
    case r: TRectSegment => rectConversor(r)
    case c: TCircleSegment => arcCircConversor(c)
    case f: TFaintElement => faintElementConversor(f)
    case _ => throw new IllegalStateException()
  }




  def elementConversor(w: WithProgresive): Seq[Node] = drawableConvert2Nodes(w)

  import UtilTransformers.PointTransformer._
  private def rectConversor(rect: TRectSegment): Seq[Node] = {
    List(new Line() {
      startX <== rect.originPoint.x.toView_X()
      startY <== rect.originPoint.y.toView_Y()
      endX <== rect.endPoint.x.toView_X()
      endY <== rect.endPoint.y.toView_Y()
      strokeWidth = 4
      stroke = Color.Blue
      fill = Color.Transparent
      //transforms ++= List(tra)
    })
  }
  private def arcCircConversor(circ: TCircleSegment): Seq[Node] = {
    List(new Arc() {
      centerX <== circ.centerPoint.x.toView_X()
      centerY <== circ.centerPoint.y.toView_Y()
      radiusX <== DoubleProperty(circ.radius)/factor
      radiusY <== DoubleProperty(circ.radius)/factor
      startAngle = -circ.initialAngle * 180 / Math.PI
      length = -circ.alpha * 180.0 / Math.PI


      strokeWidth = 4
      stroke = Color.Blue
      fill = Color.Transparent
      `type` = ArcType.Open
      //transforms ++= List(tra)
    })
  }

  private def faintElementConversor(faintElement: TFaintElement): Seq[Node] = {
    List(new Line(){
      startX <== faintElement.from.x.toView_X()
      startY <== faintElement.from.y.toView_Y()
      endX <== faintElement.end.x.toView_X()
      endY <== faintElement.end.y.toView_Y()
      strokeWidth = 5
      strokeDashArray = List(25d, 20d, 5d, 20d)
      //transforms ++= List(tra)
    })
  }



}


