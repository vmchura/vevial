package Layers

import elementdata.IRIElementData
import relevamiento.RelevamientoIRI
import scalafx.beans.property.DoubleProperty
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle

class SimpleRelevamientoLayer(relevamientoIRI: RelevamientoIRI) extends TLayer[IRIElementData] {
  override def conversor(e: IRIElementData): Seq[Node] = SimpleRelevamientoLayer.convert(e)
  addAll(relevamientoIRI.elements)
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
object SimpleRelevamientoLayer{
  import UtilTransformers.PointTransformer._
  def convert(e: IRIElementData): Seq[Node] = {

    (for{
      p <- e.point
    }yield{
      List(new Circle(){
        centerX <== p.value.x.toView_X()
        centerY <== p.value.y.toView_Y()
        radius = 5
        fill = Color.Blue
      })
    }).getOrElse{
      Nil
    }

  }
}
