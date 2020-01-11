package gym.evialgame

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.PointPlanarVector


sealed trait TSample extends Ordered[TSample]

sealed trait SampleWithProg extends TSample{
  def prog: Int
  override def compare(that: TSample): Int = that match {
    case SampleVector(thatProg,_,_) => prog.compareTo(thatProg)
    case ExactVector(thatProg) =>prog.compareTo(thatProg)
    case UndefinedSample => -1
  }

}
/**
  *
  * @param prog, aprox prog projection of the sample
  * @param distance, distance with sign to the sample
  */
case class SampleVector(prog: Int, distance: Int, toSource: PointPlanarVector) extends SampleWithProg
case class ExactVector(prog: Int) extends SampleWithProg

object UndefinedSample extends TSample {
  override def compare(that: TSample): Int = that match {
    case UndefinedSample => 0
    case _ => 1
  }
}



