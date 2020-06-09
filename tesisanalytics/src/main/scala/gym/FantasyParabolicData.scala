package gym

case class FantasyParabolicData(x: Float, y: Float) extends DataDivisble {
  override def value: Float = y

  override def toCompare: Double = x
}
