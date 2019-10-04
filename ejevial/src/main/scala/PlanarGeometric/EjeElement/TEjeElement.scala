package PlanarGeometric.EjeElement

import PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector}
import PlanarGeometric.ConfigParametersGeometric.areCloseInLinearReference



trait TEjeElement{
  def length: Double
  def in: PointUnitaryVector
  def out: PointUnitaryVector
  def ==?(o: TEjeElement): Boolean = areCloseInLinearReference(length,o.length) && in ==? o.in && out ==?o.out
  //def ++(ej: ElementoEje[_]):SecuenciaElementosEje = SecuenciaElementosEje(this,ej)
  //def findPuntoConProgresiva(progresiva: Double): Option[TPuntoProgresiva]
  //def findPuntoProgresivaPerpendicular(progresiva: Double): Option[(Punto,Punto)]
  def projectPoint(point: Point): Option[ElementPoint]
  def pointIsInsideElement(point: Point): Boolean
  def lengthToPoint(point: ElementPoint): Double
  def leftmostPoint: Point
  def rightmostPoint: Point
  def upperPoint: Point
  def lowerPoint: Point

}



trait TSimpleEjeElement extends TEjeElement


