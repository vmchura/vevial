package Layers
import UtilTransformers.PointTransformer
import scalafx.beans.property.DoubleProperty
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.Line

class DummyLayer() extends LayerBuilder[((Double, Double), (Double, Double))]{
  override def build(pointTransformer: PointTransformer) : TLayer[((Double, Double), (Double, Double))] = new TLayer[((Double, Double), (Double, Double))] {
    import pointTransformer.{DoubleXView, DoubleYView}
    override def conversor(e: ((Double, Double), (Double, Double))): Seq[Node] = {
      e match {
        case ((a, b), (x, y)) => new Line {
          startX <== a.toView_X()
          startY <== b.toView_Y()
          endX <== x.toView_X()
          endY <== y.toView_Y()
          strokeWidth = 2
          fill = Color.Black
          stroke = Color.Black
        } :: Nil
      }

    }

    addAll(List(
      ((1,2), (100, 200)),
      ((1,2), (-100, -200)),
      ((100,200), (500, -200))
    ))

    override def setListenerPanelUpdate(x: DoubleProperty, y: DoubleProperty, u: DoubleProperty, v: DoubleProperty): Unit = ()
}


}
