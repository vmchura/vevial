package Layers

import io.vmchura.vevial.PlanarGeometric.EjeElement.{TCircleSegment, TEjeElement, TFaintElement, TRectSegment}
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.TEfficientSeqEjeElementsProgresiva
import scalafx.beans.property.DoubleProperty
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Arc, ArcType, Line}
import UtilTransformers.PointTransformer
class EjeVialLayer(eje: TEfficientSeqEjeElementsProgresiva) extends LayerBuilder[TEjeElement] {

  override def build(pointTransformer: PointTransformer): TLayer[TEjeElement] = new TLayer[TEjeElement]{
    import pointTransformer.{DoubleXView, DoubleYView}

    private val drawableConvert2Nodes: TEjeElement => Seq[Node] = {
      case r: TRectSegment => rectConversor(r)
      case c: TCircleSegment => arcCircConversor(c)
      case f: TFaintElement => faintElementConversor(f)
      case _ => throw new IllegalStateException()
    }


    def elementConversor(w: TEjeElement): Seq[Node] = drawableConvert2Nodes(w)


    private def rectConversor(rect: TRectSegment): Seq[Node] = {
      List(new Line() {
        startX <== rect.originPoint.x.toView_X()
        startY <== rect.originPoint.y.toView_Y()
        endX <== rect.endPoint.x.toView_X()
        endY <== rect.endPoint.y.toView_Y()
        strokeWidth = 3
        stroke = Color.Black
      })
    }

    private def arcCircConversor(circ: TCircleSegment): Seq[Node] = {
      List(new Arc() {
        centerX <== circ.centerPoint.x.toView_X()
        centerY <== circ.centerPoint.y.toView_Y()
        radiusX <== DoubleProperty(circ.radius) / pointTransformer.factor
        radiusY <== DoubleProperty(circ.radius) / pointTransformer.factor
        startAngle = circ.initialAngle * 180 / Math.PI
        length = circ.alpha * 180.0 / Math.PI


        strokeWidth = 3
        stroke = Color.Black
        `type` = ArcType.Open
        fill = Color.Transparent
      })
    }

    private def faintElementConversor(faintElement: TFaintElement): Seq[Node] = {
      List(new Line() {
        startX <== faintElement.from.x.toView_X()
        startY <== faintElement.from.y.toView_Y()
        endX <== faintElement.end.x.toView_X()
        endY <== faintElement.end.y.toView_Y()
        strokeWidth = 2
        stroke = Color.Red
        strokeDashArray = List(25d, 20d, 5d, 20d)
      })
    }

    override def setListenerPanelUpdate(x: DoubleProperty, y: DoubleProperty, u: DoubleProperty, v: DoubleProperty): Unit = ()

    override def conversor(e: TEjeElement): Seq[Node] = elementConversor(e)

    clear()
    addAll(eje.elements)
  }

  override def minimumX: Double = eje.elements.map(_.in.point.x).min

  override def minimumY: Double = eje.elements.map(_.in.point.y).min

  override def representativeScale(mapWidth: Double, mapHeight: Double): Double = {
    val viewWidth = eje.elements.map(_.in.point.x).max - eje.elements.map(_.in.point.x).min
    val viewHeight= eje.elements.map(_.in.point.y).max - eje.elements.map(_.in.point.y).min
    (viewWidth/mapWidth) min (viewHeight/mapHeight)
  }
}
