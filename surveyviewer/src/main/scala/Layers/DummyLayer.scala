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
      ((0, 0), (100, 100)),
      ((0, 0), (-200, -200)),
      ((0, 0), (300, -300)),
      ((0, 0), (-400, 400)),
    ))

    override def setListenerPanelUpdate(x: DoubleProperty, y: DoubleProperty, u: DoubleProperty, v: DoubleProperty): Unit = ()
}

  override def minimumX: Double = -400.0

  override def minimumY: Double = -400.0

  override def representativeScale(mapWidth: Double, mapHeight: Double): Double = {
    (700/mapWidth).min(700/mapHeight)
  }
}
