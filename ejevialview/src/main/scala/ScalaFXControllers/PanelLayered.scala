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
  var globalMinX: Double = Double.MaxValue
  var globalMinY: Double = Double.MaxValue
  var globalMaxX: Double = Double.MinValue
  var globalMaxY: Double = Double.MinValue

  val pointTransformer = new PointTransformer(delegate.widthProperty(), delegate.heightProperty())
  delegate.setBackground(new Background(
    Array(
      new BackgroundFill(Color.DarkBlue, CornerRadii.Empty, Insets.Empty)
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
    val minX = layer.minimumX
    val maxX = layer.maximumX
    val minY = layer.minimumY
    val maxY = layer.maximumY

    globalMinX = globalMinX.min(minX)
    globalMinY = globalMinY.min(minY)
    globalMaxX = globalMaxX.max(maxX)
    globalMaxY = globalMaxY.max(maxY)

    val factor = ((maxX - minX)/delegate.getWidth).max((maxY - minY)/delegate.getHeight)
    println(s"$minX -> $maxX, $minY -> $maxY => $factor")
    pointTransformer.factor() = factor
    pointTransformer.offsetX() = (maxX + minX - delegate.getWidth*factor)/2.0
    pointTransformer.offsetY() = (maxY + minY - delegate.getHeight*factor)/2.0+delegate.getHeight*factor
    new ObservableListDelegate(
      Array(layer.build(pointTransformer).nodes),
      children
    )

  }

  def scaleAll(): Unit = {
    val factor = ((globalMaxX - globalMinX) / delegate.getWidth).max((globalMaxY - globalMinY) / delegate.getHeight)
    pointTransformer.factor() = factor
    pointTransformer.offsetX() = (globalMaxX + globalMinX - delegate.getWidth * factor) / 2.0
    pointTransformer.offsetY() = (globalMaxY + globalMinY - delegate.getHeight * factor) / 2.0 + delegate.getHeight * factor
  }



}
