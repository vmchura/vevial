package UtilTransformers


import UtilTransformers.PointTransformer.ViewFactor
import scalafx.beans.binding.NumberBinding
import scalafx.beans.property.{DoubleProperty, ReadOnlyDoubleProperty, ReadOnlyStringProperty, StringProperty}
import scalafx.Includes._

class PointTransformer(mapWidth: ReadOnlyDoubleProperty, mapHeight: ReadOnlyDoubleProperty) {



  val offsetX: DoubleProperty= DoubleProperty(0d)
  val offsetY: DoubleProperty= DoubleProperty(0d)
  val endX: DoubleProperty = new DoubleProperty()
  val iniY: DoubleProperty = new DoubleProperty()



  val factor: DoubleProperty = DoubleProperty(1d)
  val viewTextProperty: StringProperty = new StringProperty(this, "viewLabel", "Unknown")
  factor.onChange((_, _, newFactor) => {
    viewTextProperty.value = s"${ViewFactor(newFactor.doubleValue()).toString} // ${newFactor.toString}"
  })

  implicit class DoubleXView(x: Double){
    def toView_X: NumberBinding =convertXReal2View(x)
  }
  implicit class DoubleYView(y: Double){
    def toView_Y: NumberBinding =convertYReal2View(y)
  }

  def convertXReal2View(x: Double): NumberBinding = {
    (offsetX.multiply(-1d)+x).divide(factor)


  }
  def convertYReal2View(y: Double): NumberBinding = {
    (offsetY-y).divide(factor)

  }



  def convertXView2Real(xView: Double): Double = {
    xView*factor()+offsetX()
  }

  def convertYView2Real(yView: Double): Double = {
    -yView*factor()+offsetY()
  }
  def convertXView2Real(xView: ReadOnlyDoubleProperty): NumberBinding = {
    xView*factor+offsetX
  }

  def convertYView2Real(yView: ReadOnlyDoubleProperty): NumberBinding = {
    -yView*factor+offsetY
  }


  def updateOffsetWithPivot(newFactor: Double, pivotX: Double,pivotY: Double): Unit = {
    val oxPrime: Double = (offsetX()-pivotX)*newFactor/factor()+pivotX
    val oyPrime: Double = (offsetY()-pivotY)*newFactor/factor()+pivotY
    offsetX() = oxPrime
    offsetY() = oyPrime
    factor() = newFactor
  }

  def zoomPositive(positionXRelative: Double, positionYRelative: Double): Unit = {
    val newFactor = factor() * Math.log(2.0)
    zoom(newFactor, positionXRelative: Double, positionYRelative: Double)
  }

  def zoomNegative(positionXRelative: Double, positionYRelative: Double): Unit = {
    val newFactor = factor() / Math.log(2.0)
    zoom(newFactor, positionXRelative: Double, positionYRelative: Double)
  }
  private def zoom(newFactor: Double, positionXRelative: Double, positionYRelative: Double): Unit = {
    val px = convertXView2Real(positionXRelative)
    val py = convertYView2Real(positionYRelative)
    updateOffsetWithPivot(newFactor, px, py)
  }

  endX.unbind()
  iniY.unbind()
  endX <== convertXView2Real(mapWidth)
  iniY <== convertYView2Real(mapHeight)


}

object PointTransformer {
  sealed trait ViewFactor

  case object GlobalView extends ViewFactor

  case object MediumView extends ViewFactor

  case object LocalView extends ViewFactor

  object ViewFactor {
    def apply(factor: Double): ViewFactor = {
      val scale = math.log(factor)
      if (scale > 5) {
        GlobalView
      }else{
        if(scale > 4){
          MediumView
        }else{
          LocalView
        }
      }
    }
  }

}