package Layers

import models.LinkGeo
import scalafx.beans.property.DoubleProperty
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.Line
import UtilTransformers.PointTransformer._

class LinkLayer extends TLayer[LinkGeo]{
  override def conversor(e: LinkGeo): Seq[Node] = {
    List(new Line(){
      startX <== e.originPoint.center.x.toView_X()
      startY <== e.originPoint.center.y.toView_Y()
      endX <== e.endPoint.center.x.toView_X()
      endY <== e.endPoint.center.y.toView_Y()

      stroke = Color.OrangeRed
      strokeWidth = 0.5
    })
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
