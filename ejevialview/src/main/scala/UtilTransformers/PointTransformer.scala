package UtilTransformers


import scalafx.beans.binding.NumberBinding
import scalafx.beans.property.{DoubleProperty, ReadOnlyDoubleProperty}
import scalafx.Includes._

object PointTransformer {



  val offsetX: DoubleProperty= DoubleProperty(0d)
  val offsetY: DoubleProperty= DoubleProperty(0d)
  val endX: DoubleProperty= new DoubleProperty()
  val iniY: DoubleProperty= new DoubleProperty()

  val factor: DoubleProperty = DoubleProperty(1d)

  implicit class DoubleXView(x: Double){
    def toView_X(): NumberBinding =convertXReal2View(x)
  }
  implicit class DoubleYView(y: Double){
    def toView_Y(): NumberBinding =convertYReal2View(y)
  }
  /**
    *
    * @param x
    * @return
    */
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
    xView*factor()+offsetX()
  }

  def convertYView2Real(yView: ReadOnlyDoubleProperty): NumberBinding = {
    -yView*factor()+offsetY()
  }


  def updateOffsetWithPivot(newFactor: Double, pivotX: Double,pivotY: Double): Unit = {
    val oxPrime: Double = (offsetX()-pivotX)*newFactor/factor()+pivotX
    val oyPrime: Double = (offsetY()-pivotY)*newFactor/factor()+pivotY
    offsetX() = oxPrime
    offsetY() = oyPrime
    factor() = newFactor
  }



}
