package io.vmchura.vevial.PlanarGeometric.BasicGeometry

import io.vmchura.vevial.PlanarGeometric.ConfigParametersGeometric.{areCloseInAxisDistance, isValidMagnitudeForGeneralVector}


trait TPoint{
  val x: Double
  val y: Double
  //moves the point in according to vector v
  def + (v: PlanarVector): Point = Point(x+v.dx,y+v.dy)
  //moves the point in according to inverse vector v
  def - (v: PlanarVector): Point = this + (v <¬ 2)
  //calculates the Planar vector [p to this]
  def - (p: TPoint): PlanarVector = {
    val (newdx,newdy,newmagnitude) = getNewValuesToCalcVector(p)
    PlanarVector(TDirection(newdx/newmagnitude,newdy/newmagnitude),newmagnitude)
  }
  //calculates the Planar vector [p to this], use this option if maybe p is close to this
  def -? (p: TPoint): Option[PlanarVector] = {
    val (newdx,newdy,newmagnitude) = getNewValuesToCalcVector(p)
    if(isValidMagnitudeForGeneralVector(newmagnitude))
      Some(PlanarVector(TDirection(newdx/newmagnitude,newdy/newmagnitude),newmagnitude))
    else
      None
  }



  private def getNewValuesToCalcVector(p: TPoint) : (Double,Double,Double) = {
    val newdx = x - p.x
    val newdy = y - p.y
    val newmagnitude = Math.sqrt(newdx*newdx+newdy*newdy)
    (newdx,newdy,newmagnitude)
  }

  /**
    * calculates the middle point
    */

  def -%-(p: TPoint): Point = {
    val nx = (x+p.x)/2.0
    val ny = (y+p.y)/2.0
    Point(nx,ny)
  }
  //are equivalent?
  def ==? (p: TPoint): Boolean = areCloseInAxisDistance(x,p.x) && areCloseInAxisDistance(y,p.y)
  //def project (ee: ElementoEje[_]): TPuntoProgresiva
}

case class Point(x: Double, y: Double) extends TPoint
