package ShapeGenerator

import io.vmchura.vevial.PlanarGeometric.EjeElement.{TCircleSegment, TEjeElement, TFaintElement, TRectSegment}
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.WithProgresive
import UtilTransformers.PointTransformer._
import scalafx.scene.paint.{Color, Paint}
import scalafx.scene.shape.{Arc, ArcType, Line, Shape}
import scalafx.scene.transform.{Scale, Translate}
import scalafx.Includes._
import scalafx.beans.property.DoubleProperty
import scalafx.geometry.Point2D

object SimpleEjeElementConverter {

  trait ConversorToShape[A]{
    def convert(a: A): Shape
  }




  implicit val faintConversor = new ConversorToShape[TFaintElement] {
    override def convert(a: TFaintElement): Shape = new Line(){
      startX <== convertXReal2View(a.from.x)
      startY <== convertYReal2View(a.from.y)
      endX <== convertXReal2View(a.end.y)
      endY <== convertYReal2View(a.end.y)
      strokeWidth = 5
      strokeDashArray = List(25d, 20d, 5d, 20d)
      //transforms ++= List(tra)
    }
  }
  private def specificConverterToShape[A](a: A)(implicit conv: ConversorToShape[A]): Shape = conv.convert(a)

  /*
  def convertToShape(a: WithProgresive): Shape = a match {
    case r: TRectSegment => specificConverterToShape(r.asInstanceOf[TRectSegment])
    case c: TCircleSegment => specificConverterToShape(c.asInstanceOf[TCircleSegment])
    case f: TFaintElement => specificConverterToShape(f.asInstanceOf[TFaintElement])
    case _ => throw new IllegalStateException()
  }

   */




}
