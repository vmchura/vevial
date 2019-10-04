package PlanarGeometric


import PlanarGeometric.BasicEje.SubsequenceFinder
import PlanarGeometric.BasicEje.SubsequenceFinder._
import org.scalatest.FlatSpec

class SubsequenceFinderTest extends FlatSpec {

  "Find on binary search with simple array of ints" should " find correctly values" in {

    val a = (0 to 100 by 10).toArray
    implicit def f: Int => Double = (x: Int) => x
    val offsetLeft = 15
    val offsetRight = 15
    val s = SubsequenceFinder.find[Int](offsetLeft,offsetRight)(a)(50)
    assertResult(Set(40,50,60))(s)

    val s2 = SubsequenceFinder.find[Int](offsetLeft,offsetRight)(a) _
    assertResult(Some(49,51))(s2(50))
    assertResult(Some(59,61))(s2(60))
    assertResult(Some(39,41))(s2(40))

  }
  "Find on binary search on array of tuples " should " find correctly values" in {

    case class Tuple(a: Double, b: Double)
    val a = (0 to 100 by 10).toArray.map(i => Tuple(i,-i))
    implicit def firstExtractor: Tuple => Double = _.a
    implicit def secondExtractor: Tuple => Double = _.b
    val offsetLeft = 15.0
    val offsetRight = 15.0

    val aOrderByX = a.sortBy(_.a)
    val aOrderByY = a.sortBy(_.b)

    def finderByX(d: Double) = SubsequenceFinder.find[Tuple](offsetLeft,offsetRight)(aOrderByX) (d) (firstExtractor)
    def finderByY(d: Double) = SubsequenceFinder.find[Tuple](offsetLeft,offsetRight)(aOrderByY) (d) (secondExtractor)


    assertResult(Some((49,51)))(finderByX(50.0))
    assertResult(Some((59,61)))(finderByX(60.0))

    assertResult(Some(((100 - 50 - 1),(100-50+1))))(finderByY(-50.0))
    assertResult(Some(((100 - 60 - 1),(100-60+1))))(finderByY(-60.0))

  }

}
