package ShapeGenerator

import io.vmchura.vevial.PlanarGeometric.EjeElement.TRectSegment
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Arc, ArcType, Shape}

object EjeConverter {

  def apply(eje: EfficientEjeProgresiva): Seq[Shape] = {
    Nil
    //val arrayShapes = eje.elements.map{SimpleEjeElementConverter.convertToShape}
    //arrayShapes
    //val s = arrayShapes.reduceLeft((a,b) => Shape.union(a,b))
//    s.setFill(Color.Blue)
  //  s.setStrokeWidth(5.0)
    //s

  }
}
