package gym

trait DataDivisble extends Comparable[Double]{
  def value: Float
  def toCompare: Double
  override def compareTo(t: Double): Int = toCompare.compareTo(t)
}
