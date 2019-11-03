package io.vmchura.vevial.elementdata


trait TElementData[A <: TElementData[A]] { this: A =>
  def point: Option[UPoint]
  def speed: Option[UDouble]
  def vectorToNext: Option[UPlanarVector]
  def vectorToPrev: Option[UPlanarVector]
  def withNextElement(a: A): A
  def withPrevElement(a: A): A
}
