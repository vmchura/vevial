package Layers

import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.TEfficientSeqEjeElementsProgresiva
import io.vmchura.vevial.elementdata.IRIElementData
import io.vmchura.vevial.relevamiento.RelevamientoIRI
import scalafx.beans.property.DoubleProperty
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line}
import UtilTransformers.PointTransformer._

class ProjectionIRIRelevamientoLayer(relevamientoIRI: RelevamientoIRI,eje: TEfficientSeqEjeElementsProgresiva) extends TLayer[IRIElementData] {
  override def conversor(e: IRIElementData): Seq[Node] = {
    (for{
      p <- e.point
    }yield{

      val c = new Circle() {
        centerX <== p.value.x.toView_X()
        centerY <== p.value.y.toView_Y()
        radius = 3
        fill = Color.Red
      }
      eje.projectPoint(p.value).flatMap{ ep=>

          c.fill() = Color.Blue

          ep.toSource.map{ _ =>

            List(c,
            new Line(){
              startX <== ep.x.toView_X()
              startY <== ep.y.toView_Y()
              endX <== p.value.x.toView_X()
              endY <== p.value.y.toView_Y()
              strokeWidth = 1
              stroke = Color.Blue

            })
          }
      }.getOrElse(Nil)



    }).getOrElse(Nil)



  }
  addAll(relevamientoIRI.elements)
  /**
    * update nodes drawn, (x,y) top left corner (u,v) bottom right corner
    */
  override def setListenerPanelUpdate(x: DoubleProperty, y: DoubleProperty, u: DoubleProperty, v: DoubleProperty): Unit = ()
}


