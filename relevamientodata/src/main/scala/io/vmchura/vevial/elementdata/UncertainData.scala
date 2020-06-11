package io.vmchura.vevial.elementdata

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{PlanarVector, Point, TDirection, TPoint}

/**
  * https://upload.wikimedia.org/wikipedia/commons/thumb/7/74/Normal_Distribution_PDF.svg/1920px-Normal_Distribution_PDF.svg.png
  * value: mu or u, media
  * sigma2: sigmasquare
 *
  * @tparam A: type of data
  */
sealed trait UncertainData[A,B] { self: B =>
  def value: A
  def sigma2: Double
  def |-| (other: B): B
}

object UncertainData{
  def merge(x: UDouble,y : UDouble): UDouble = {
    (x.sigma2,y.sigma2) match {
      case (Double.PositiveInfinity,_) => y
      case (_,Double.PositiveInfinity) => x
      case (sx,sy) =>
        val u = (x.value*sy + y.value*sx)/(sx+sy)
        UDouble(u,(sx*sy)/(sx+sy))
    }
  }
}

case class UPoint(value: TPoint, sigma2: Double) extends UncertainData[TPoint,UPoint] {
  override def |-|(other: UPoint): UPoint = {
    val x = UDouble(value.x,sigma2) |-| UDouble(other.value.x,other.sigma2)
    val y = UDouble(value.y,sigma2) |-| UDouble(other.value.y,other.sigma2)
    val p = Point(x.value,y.value)
    UPoint(p,x.sigma2)
  }
}

case class UDouble(value: Double, sigma2: Double) extends  UncertainData[Double,UDouble] {
  override def |-|(other: UDouble): UDouble = UncertainData.merge(this,other)
}

case class UDirection(value: TDirection, sigma2: Double) extends  UncertainData[TDirection,UDirection] {
  override def |-|(other: UDirection): UDirection =  {
    val dx = UDouble(value.dx,sigma2) |-| UDouble(other.value.dx,other.sigma2)
    val dy = UDouble(value.dy,sigma2) |-| UDouble(other.value.dy,other.sigma2)
    val s = Math.sqrt(dx.value*dx.value+dy.value*dy.value)
    if(s < 1e-6){
      UDirection(TDirection(),Double.PositiveInfinity)
    }else{
      val p = TDirection(dx.value/s,dy.value/s)
      UDirection(p,dx.sigma2)
    }

  }
}

case class UPlanarVector(value: PlanarVector, sigma2: Double) extends UncertainData[PlanarVector,UPlanarVector] {
  override def |-|(other: UPlanarVector): UPlanarVector = {
    val d = UDirection(value.direction,sigma2) |-| UDirection(other.value.direction,other.sigma2)
    val m = UDouble(value.magnitude,sigma2) |-| UDouble(other.value.magnitude,other.sigma2)

    UPlanarVector(PlanarVector(d.value,m.value),d.sigma2)
  }
}

