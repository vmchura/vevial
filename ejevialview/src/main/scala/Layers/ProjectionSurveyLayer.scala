package Layers

import UtilTransformers.PointTransformer
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.TEfficientSeqEjeElementsProgresiva
import io.vmchura.vevial.elementdata.TElementData
import io.vmchura.vevial.relevamiento.Survey
import scalafx.beans.property.DoubleProperty
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line}

class ProjectionSurveyLayer[T <: TElementData[T]](survey: Survey[T], roadAxis: TEfficientSeqEjeElementsProgresiva)
  extends LayerBuilder[T]{
  override def build(pointTransformer: PointTransformer): TLayer[T] = new TLayer[T]{
    import pointTransformer.{DoubleXView, DoubleYView}
    override def conversor(e: T): Seq[Node] = {
      val projectionPoint: Option[TPoint] = e.point.flatMap(originPoint => roadAxis.projectPoint(originPoint.value).map(_.point))
      val origin = e.point.map{ originPoint =>
        new Circle() {
          centerX <== originPoint.value.x.toView_X()
          centerY <== originPoint.value.y.toView_Y()
          radius = 3
          fill = projectionPoint.map(_ => Color.Blue).getOrElse(Color.Red)
        }
      }

      val line = for {
        originPoint <- e.point
        projection <- projectionPoint
      }yield {
        new Line() {
          startX <== projection.x.toView_X()
          startY <== projection.y.toView_Y()
          endX <== originPoint.value.x.toView_X()
          endY <== originPoint.value.y.toView_Y()
          strokeWidth = 1
          stroke = Color.Blue

        }
      }

      List(origin, line).flatten
    }

    addAll(survey.surveyInformation)

    override def setListenerPanelUpdate(x: DoubleProperty, y: DoubleProperty, u: DoubleProperty, v: DoubleProperty): Unit = ()
}

  override def minimumX: Double = survey.surveyInformation.flatMap(_.point.map(_.value.x)).min

  override def minimumY: Double = survey.surveyInformation.flatMap(_.point.map(_.value.y)).min

  override def maximumX: Double = survey.surveyInformation.flatMap(_.point.map(_.value.x)).max

  override def maximumY: Double = survey.surveyInformation.flatMap(_.point.map(_.value.y)).max
}
