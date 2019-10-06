package analizetramos

import PlanarGeometric.BasicEje.TEfficientSeqEjeElements
import PlanarGeometric.EjeElement.{TCircleSegment, TEjeElement, TRectSegment}
import RFunctionDefiner.PlotHistogram

class CalculatorLengthElements(val elements: List[TEjeElement]){
  import RFunctionDefiner.RInitializer.R

  def this(eje: TEfficientSeqEjeElements) = this(eje.elements)
  def ++ (o: CalculatorLengthElements): CalculatorLengthElements = new CalculatorLengthElements(elements ++ o.elements)
  def calcAllLengthsAndSaveGraph(path: String): Unit ={
    require(path.endsWith(".png"))
    PlotHistogram.drawAndSaveHistogram(elements.map(_.length).toArray,
      "Histograma de longitudes de elementos","Longitud de elementos",
      0,250,1,
      path)
  }
  def calcRectElementAndSaveGraph(path: String): Unit ={
    require(path.endsWith(".png"))
    PlotHistogram.drawAndSaveHistogram(elements.filter{
      case _: TRectSegment => true
      case _ => false
    }.map(_.length).toArray,
      "Histograma de longitudes de elementos lineales","Longitud de elementos lineales",
      0,250,1,
      path)
  }
  def calcCircElementAndSaveGraph(path: String): Unit ={
    require(path.endsWith(".png"))
    PlotHistogram.drawAndSaveHistogram(elements.filter{
      case _: TCircleSegment => true
      case _ => false
    }.map(_.length).toArray,
      "Histograma de longitudes de elementos circulares","Longitud de elementos circulares",
      0,250,1,
      path)
  }
  def calcCircAngleElementAndSaveGraph(path: String): Unit ={
    require(path.endsWith(".png"))
    PlotHistogram.drawAndSaveHistogram(elements.flatMap {
      case c: TCircleSegment => Some(c.alpha)
      case _ => None
    }.toArray,
      "Histograma de longitudes de elementos circulares","Longitud de elementos circulares",
      -3.2,3.2,0.05,
      path)
  }
  def calcCircAngleLengthElementAndSaveGraph(path: String): Unit ={
    require(path.endsWith(".png"))
    val data = elements.flatMap {
      case c: TCircleSegment => Some((c.alpha,c.length))
      case _ => None
    }.toArray

    PlotHistogram.drawAndSaveHistogram2D(data.map(_._1),data.map(_._2),
      "Histograma de longitud y angulo de elementos circulares","Angulo","Longitud",
      path)
  }

}
