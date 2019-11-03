package io.vmchura.vevial.elementdata

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{PlanarVector, Point, TDirection, TPoint}

/**
  * https://upload.wikimedia.org/wikipedia/commons/thumb/7/74/Normal_Distribution_PDF.svg/1920px-Normal_Distribution_PDF.svg.png
  * value: mu or u, media
  * sigma2: sigmasquare
 *
  * @tparam A: type of data
  */
sealed trait UncertainData[A] {
  def value: A
  def sigma2: Double
}



case class UPoint(value: Point, sigma2: Double) extends UncertainData[Point]

case class UDouble(value: Double, sigma2: Double) extends  UncertainData[Double]

case class UDirection(value: TDirection, sigma2: Double) extends  UncertainData[TDirection]

case class UPlanarVector(value: PlanarVector, sigma2: Double) extends UncertainData[PlanarVector]

