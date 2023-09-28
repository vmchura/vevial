package io.vmchura.vevial.PlanarGeometric

object ConfigParametersGeometric {
  def isMagnitudeValidForUnitaryVector(magnitude: Double): Boolean =
    Math.abs(magnitude - 1.0) < 1e-4
  def isValidMagnitudeForGeneralVector(magnitude: Double): Boolean =
    magnitude > 1e-4
  def areCloseValuesForDirection(u: Double, v: Double): Boolean =
    Math.abs(u - v) < 1e-4
  def areCloseValuesForMagnitude(d: Double, t: Double): Boolean =
    Math.abs(d - t) < 1e-4
  def areCloseInAxisDistance(ux: Double, vx: Double): Boolean =
    Math.abs(ux - vx) < 1e-4
  def areCloseInLinearReference(p1: Double, p2: Double): Boolean =
    Math.abs(p1 - p2) < 1e-4
  val distanceToFindProjection = 200.0 //metres
}
