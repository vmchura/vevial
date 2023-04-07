package Layers

import UtilTransformers.PointTransformer
import UtilTransformers.PointTransformer.{GlobalView, LocalView, MediumView}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.TEfficientSeqEjeElementsProgresiva
import io.vmchura.vevial.elementdata.TElementData
import io.vmchura.vevial.relevamiento.Survey
import scalafx.beans.property.DoubleProperty
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line}

class SurveyAlteredLayer[T <: TElementData[T]](survey: Survey[T],
                                               correctionX: DoubleProperty,
                                               correctionY: DoubleProperty)
  extends LayerBuilder[T]{
  override def build(pointTransformer: PointTransformer): TLayer[T] = new TLayer[T]{
    import pointTransformer.{DoublePropertyXView, DoublePropertyYView}

    override def conversor(e: T): Seq[Node] = {
      val origin = e.point.map{ originPoint =>
        new Circle() {
          centerX <== (correctionX + originPoint.value.x).toView_X
          centerY <== (correctionY + originPoint.value.y).toView_Y
          radius = 3
          fill = Color.LightGreen
        }
      }
      List(origin).flatten
    }

    pointTransformer.offsetX.onChange((_, _, _) => update())
    update()

    override def update(): Unit = {
      val module = PointTransformer.ViewFactor(pointTransformer.factor()) match {
        case GlobalView => 10
        case MediumView => 5
        case LocalView => 1
      }
      val minimumX = pointTransformer.offsetX()
      val minimumY = pointTransformer.iniY()
      val maximumX = pointTransformer.endX()
      val maximumY = pointTransformer.offsetY()
      val elementsToPlot = survey.surveyInformation.zipWithIndex.filter{
        case (element, index) =>
          val byZoom = index % module == 0
          val byMinimumMaximumView = element.point.map(_.value).exists(p => minimumX <= p.x && p.x <= maximumX && minimumY <= p.y && p.y <= maximumY)
          byZoom && byMinimumMaximumView
      }.map(_._1).toSet
      draw(elementsToPlot)
    }
}

  override def minimumX: Double = survey.surveyInformation.flatMap(_.point.map(_.value.x)).min

  override def minimumY: Double = survey.surveyInformation.flatMap(_.point.map(_.value.y)).min

  override def maximumX: Double = survey.surveyInformation.flatMap(_.point.map(_.value.x)).max

  override def maximumY: Double = survey.surveyInformation.flatMap(_.point.map(_.value.y)).max
}
