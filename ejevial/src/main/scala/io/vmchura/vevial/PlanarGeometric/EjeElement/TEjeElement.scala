package io.vmchura.vevial.PlanarGeometric.EjeElement

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TPoint}
import io.vmchura.vevial.PlanarGeometric.ConfigParametersGeometric.areCloseInLinearReference



trait TEjeElement{
  def length: Double
  def in: PointUnitaryVector
  def out: PointUnitaryVector
  def ==?(o: TEjeElement): Boolean = areCloseInLinearReference(length,o.length) && in ==? o.in && out ==?o.out
  //def ++(ej: ElementoEje[_]):SecuenciaElementosEje = SecuenciaElementosEje(this,ej)
  //def findPuntoConProgresiva(progresiva: Double): Option[TPuntoProgresiva]
  //def findPuntoProgresivaPerpendicular(progresiva: Double): Option[(Punto,Punto)]
  def projectPoint(point: TPoint): Option[ElementPoint]
  def pointIsInsideElement(point: TPoint): Boolean
  def lengthToPoint(point: ElementPoint): Double
  def leftmostPoint: TPoint
  def rightmostPoint: TPoint
  def upperPoint: TPoint
  def lowerPoint: TPoint

}



trait TSimpleEjeElement extends TEjeElement


