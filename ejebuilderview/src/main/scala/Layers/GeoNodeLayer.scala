package Layers

import models.GeoNode
import scalafx.beans.property.DoubleProperty
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import UtilTransformers.PointTransformer._

class GeoNodeLayer extends TLayer[GeoNode]{
  override def conversor(e: GeoNode): Seq[Node] = {
    val xParam = e.center.x.toView_X()
    val yParam = e.center.y.toView_Y()
    List(new Circle(){
      centerX <== xParam
      centerY <== yParam
      radius = 5
      fill = Color.OrangeRed
    }
      /*
      ,new Text(){
        text = e._2.toString
        x <== xParam
        y <== yParam
        font = Font(24)
        stroke = Color.Black
      }

       */
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
