package ScalaFXControllers
import Layers.{LayerBuilder, ObservableListDelegate}
import UtilTransformers.PointTransformer
import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty, ObjectProperty}
import scalafx.geometry.Insets
import scalafx.scene.layout.{Background, BackgroundFill, CornerRadii, Pane}
import scalafx.scene.paint.Color
import javafx.scene.{layout => jfxsl}
import javafx.{scene => jfxs}
class PanelLayered(override val delegate: jfxsl.Pane = new jfxsl.Pane) extends Pane(delegate){
  val pointTransformer = new PointTransformer(delegate.widthProperty(), delegate.heightProperty())
  delegate.setBackground(new Background(
    Array(
      new BackgroundFill(Color.LightGray, CornerRadii.Empty, Insets.Empty)
    )
  ))
  delegate.onScroll = ae => {
    ae.getDeltaY.sign match {
      case sign: Double if sign > 0.0 => pointTransformer.zoomPositive(ae.getX, ae.getY)
      case sign: Double if sign < 0.0 => pointTransformer.zoomNegative(ae.getX, ae.getY)
      case _ => ()
    }
  }
  def appendLayer[T](layer: LayerBuilder[T]): Unit = {
    new ObservableListDelegate(
      Array(layer.build(pointTransformer).nodes),
      children
    )

  }



}
