package Layers

import io.vmchura.vevial.PlanarGeometric.EjeElement.ElementPoint
import scalafx.beans.property.DoubleProperty
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line}
import UtilTransformers.PointTransformer._
class ProjectionPointLayer extends TLayer[(ElementPoint,Boolean)]{
  override def conversor(eparam: (ElementPoint,Boolean)): Seq[Node] = {
    val (e,isSpecial) = eparam
    val xStartParam = e.x.toView_X()
    val yStartParam = e.y.toView_Y()
    val xEndParam = e.sourcePoint.x.toView_X()
    val yEndParam = e.sourcePoint.y.toView_Y()
    List(new Circle(){
      centerX <== xStartParam
      centerY <== yStartParam
      radius = 3
      fill = if(isSpecial) Color.Red else Color.DarkGreen
    },
      new Line(){
        startX <== xStartParam
        startY <== yStartParam
        endX <== xEndParam
        endY <== yEndParam
        stroke = if(isSpecial) Color.Red else Color.DarkGreen
        strokeWidth = 2
      },
      new Circle(){
        centerX <== xEndParam
        centerY <== yEndParam
        radius = 3
        fill = if(isSpecial) Color.Red else Color.DarkGreen
      }
    )

  }



  /**
    * update nodes drawn, (x,y) top left corner (u,v) bottom right corner
    *
    * @param x
    * @param y
    * @param u
    * @param v
    */
  override def setListenerPanelUpdate(x: DoubleProperty, y: DoubleProperty, u: DoubleProperty, v: DoubleProperty): Unit = ()
}
