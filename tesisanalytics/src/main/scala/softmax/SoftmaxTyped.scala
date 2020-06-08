package softmax

import breeze.linalg.{Vector, max, sum}
import breeze.numerics.exp

object SoftmaxTyped {
  /**
    * Calcs SoftMaxProb using trick to try to avoid overflow of exponentiation
    * @param input: Vector of Double
    * @return Vector[Double] same length of input, sum up to 1.0
    */
  def compute_softmax_prob(input: Vector[Double]): Vector[Double] = {

    /** # Set the constant c by finding the maximum of input                                          */
    val c = max(input)

    /** # Compute the numerator by subtracting c from state-action preferences and exponentiating it  */
    val numerator: Vector[Double] = exp(input-c)

    /** # Next compute the denominator by summing the values in the numerator                         */
    val denominator = sum(numerator)

    /** # Create a probability array by dividing each element in numerator array by denominator       */
    val softmax_prob: Vector[Double] = numerator/denominator
    softmax_prob
  }
  def sample(distribution: Vector[Double]): Int = {
    val p = scala.util.Random.nextDouble()
    val distributionAcummulative: Vector[Double] = distribution.scanLeft(0d)(_ + _)
    distributionAcummulative.findAll(_ >= p).minOption match {
      case Some(indx) => if(indx > 0) indx-1 else 0
      case None => throw new IllegalArgumentException("")
    }
  }
}
