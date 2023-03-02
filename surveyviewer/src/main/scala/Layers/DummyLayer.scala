package Layers
import UtilTransformers.PointTransformer
import scalafx.beans.property.DoubleProperty
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.Line

class DummyLayer(elements: List[((Double, Double), (Double, Double))]) extends LayerBuilder[((Double, Double), (Double, Double))]{
  override def build(pointTransformer: PointTransformer) : TLayer[((Double, Double), (Double, Double))] = new TLayer[((Double, Double), (Double, Double))] {
    import pointTransformer.{DoubleXView, DoubleYView}
    override def conversor(e: ((Double, Double), (Double, Double))): Seq[Node] = {
      e match {
        case ((a, b), (x, y)) => new Line {
          startX <== a.toView_X()
          startY <== b.toView_Y()
          endX <== x.toView_X()
          endY <== y.toView_Y()
          strokeWidth = 4
          fill = Color.White
          stroke = Color.White
        } :: Nil
      }

    }

    addAll(elements)

    override def setListenerPanelUpdate(x: DoubleProperty, y: DoubleProperty, u: DoubleProperty, v: DoubleProperty): Unit = ()
}

  override def minimumX: Double = elements.flatMap(x => x._1._1 :: x._2._1 :: Nil).min
  override def maximumX: Double = elements.flatMap(x => x._1._1 :: x._2._1 :: Nil).max
  override def minimumY: Double = elements.flatMap(x => x._1._2 :: x._2._2 :: Nil).min
  override def maximumY: Double = elements.flatMap(x => x._1._2 :: x._2._2 :: Nil).max

}
