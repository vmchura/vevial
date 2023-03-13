package Layers

import io.vmchura.vevial.elementdata.TElementWithPoint
import io.vmchura.vevial.relevamiento.RelevamientoIRI
import scalafx.beans.property.DoubleProperty
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle

class SimpleIRIRelevamientoLayer[T <: TElementWithPoint[T]](relevamientoIRI: RelevamientoIRI[T]) extends TLayer[T] {
  override def conversor(e: T): Seq[Node] = SimpleIRIRelevamientoLayer.convert(e)
  addAll(relevamientoIRI.elements)
  /**
    * update nodes drawn, (x,y) top left corner (u,v) bottom right corner
    *
    */
  override def update(): Unit = ()
}
object SimpleIRIRelevamientoLayer{
  import UtilTransformers.PointTransformer
  val pointTransformer = new PointTransformer(null, null)
  import pointTransformer._
  def convert[T <: TElementWithPoint[T]](e: T): Seq[Node] = {

    (for{
      p <- e.point
    }yield{
      List(new Circle(){
        centerX <== p.value.x.toView_X
        centerY <== p.value.y.toView_Y
        radius = 3
        fill = Color.Blue
      })
    }).getOrElse{
      Nil
    }

  }
}
