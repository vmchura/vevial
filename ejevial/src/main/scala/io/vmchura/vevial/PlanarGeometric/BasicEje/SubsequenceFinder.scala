package io.vmchura.vevial.PlanarGeometric.BasicEje

object SubsequenceFinder {

  trait SimpleReference[A] extends Comparable[A] {
    def <(other: A): Boolean = this.compareTo(other) < 0
    def <=(other: A): Boolean = this.compareTo(other) <= 0
    def >(other: A): Boolean = this.compareTo(other) > 0
    def >=(other: A): Boolean = this.compareTo(other) >= 0
    def -(other: A): A
    def +(other: A): A
  }

  implicit class DoubleSimpleReference(val value: Double)
      extends SimpleReference[DoubleSimpleReference] {
    override def -(other: DoubleSimpleReference): DoubleSimpleReference =
      value - other.value

    override def +(other: DoubleSimpleReference): DoubleSimpleReference =
      value + other.value

    override def compareTo(t: DoubleSimpleReference): Int =
      value.compareTo(t.value)
  }
  implicit class IntSimpleReference(val value: Int)
      extends SimpleReference[IntSimpleReference] {
    override def -(other: IntSimpleReference): IntSimpleReference =
      value - other.value

    override def +(other: IntSimpleReference): IntSimpleReference =
      value + other.value

    override def compareTo(t: IntSimpleReference): Int =
      value.compareTo(t.value)
  }

  /**
    *
    * @param a ordered
    * @param reference
    * @param offsetMin lowerLimit = reference-offsetMin
    * @param offsetMax upperLimit = reference+offsetMax
    * @tparam A
    * @return
    */
  def find[A](offsetMin: Double, offsetMax: Double)(
      a: Array[A]
  )(reference: Double)(implicit f: A => Double): Option[(Int, Int)] = {
    require(a.nonEmpty, "Array a are empty")
    for {
      from <- findLowestIndex(a, reference - offsetMin)
      to <- findTopIndex(a, reference + offsetMax)
    } yield {
      (from, to)
      //a.slice(from,to+1).toSet
    }

  }

  /**
    * returns the max index i / f(a(i)) <= maxLevel
    * @param a
    * @param maxLevel
    * @param f
    * @tparam A
    * @return
    */
  private def findTopIndex[A](a: Array[A], maxLevel: Double)(implicit
      f: A => Double
  ): Option[Int] = {
    var left = 0
    var right = a.length - 1

    if (f(a(right)) <= maxLevel)
      Some(right)
    else {
      if (f(a(left)) > maxLevel) {
        None
      } else {
        //    f(a(left)) <= maxLevel  <  f(a(right))
        while (right - left > 1) {
          val m = (left + right) / 2

          if (f(a(m)) <= maxLevel) {
            left = m
          } else {
            right = m
          }

        }

        Some(left)
      }
    }
  }

  /**
    * returns the min index i / f(a(i)) >= lowestLevel
    * @param a
    * @param lowestLevel
    * @param f
    * @tparam A
    * @return
    */
  private def findLowestIndex[A](a: Array[A], lowestLevel: Double)(implicit
      f: A => Double
  ): Option[Int] = {
    var left = 0
    var right = a.length - 1

    if (f(a(left)) >= lowestLevel) // f(a(left)) >= lowestLevel
      Some(left)
    else {
      if (f(a(right)) < lowestLevel) { // f(a(right)) < lowestLevel
        None
      } else {
        //    // f(a(left)) < lowestLevel  <=  f(a(right))
        while (right - left > 1) {
          val m = (left + right) / 2

          if (f(a(m)) >= lowestLevel) {
            right = m
          } else {
            left = m
          }

        }

        Some(right)
      }
    }
  }
}
