package PlanarGeometric


import PlanarGeometric.BasicEje.SubsequenceFinder
import PlanarGeometric.BasicEje.SubsequenceFinder._
import org.scalatest.FlatSpec

class SubsequenceFinderTest extends FlatSpec {

  "Find on binary search with simple array of ints" should " find correctly values" in {

    val a = (0 to 100 by 10).toArray
    implicit def f: Int => Int = (x: Int) => x
    val offsetLeft = 15
    val offsetRight = 15
    val s = SubsequenceFinder.find[Int,IntSimpleReference](offsetLeft,offsetRight)(a)(50)
    assertResult(Set(40,50,60))(s)

    val s2 = SubsequenceFinder.find[Int,IntSimpleReference](offsetLeft,offsetRight)(a) _
    assertResult(Set(40,50,60))(s2(50))
    assertResult(Set(50,60,70))(s2(60))
    assertResult(Set(30,40,50))(s2(40))

  }
  "Find on binary search on array of tuples " should " find correctly values" in {

    case class Tuple(a: Double, b: Double)
    val a = (0 to 100 by 10).map(i => Tuple(i,-i))
    implicit def firstExtractor: Tuple => DoubleSimpleReference = _.a
    implicit def secondExtractor: Tuple => DoubleSimpleReference = _.b
    val offsetLeft = 15.0
    val offsetRight = 15.0

    val aOrderByX = a.sortBy(_.a)
    val aOrderByY = a.sortBy(_.b)

    def finderByX(d: Double): Set[Tuple] = SubsequenceFinder.find[Tuple,DoubleSimpleReference](offsetLeft,offsetRight)(aOrderByX) (d) (firstExtractor)
    def finderByY(d: Double): Set[Tuple] = SubsequenceFinder.find[Tuple,DoubleSimpleReference](offsetLeft,offsetRight)(aOrderByY) (d) (secondExtractor)

    assertResult(Set(Tuple(40.0,-40.0),Tuple(50.0,-50.0),Tuple(60.0,-60.0)))(finderByX(50.0))
    assertResult(Set(Tuple(50.0,-50.0),Tuple(60.0,-60.0),Tuple(70.0,-70.0)))(finderByX(60.0))

    assertResult(Set(Tuple(40.0,-40.0),Tuple(50.0,-50.0),Tuple(60.0,-60.0)))(finderByY(-50.0))
    assertResult(Set(Tuple(50.0,-50.0),Tuple(60.0,-60.0),Tuple(70.0,-70.0)))(finderByY(-60.0))

    val (fX,fY) = List((firstExtractor,aOrderByX),(secondExtractor,aOrderByY)).map{case (e,lista) => (d: Double) =>
      SubsequenceFinder.find[Tuple,DoubleSimpleReference](offsetLeft,offsetRight)(lista) (d) (e)} match {
      case first :: second :: Nil => (first,second)
      case _ => (null,null)
    }
    assertResult(Set(Tuple(40.0,-40.0),Tuple(50.0,-50.0),Tuple(60.0,-60.0)))(fX(50.0))
    assertResult(Set(Tuple(50.0,-50.0),Tuple(60.0,-60.0),Tuple(70.0,-70.0)))(fX(60.0))

    assertResult(Set(Tuple(40.0,-40.0),Tuple(50.0,-50.0),Tuple(60.0,-60.0)))(fY(-50.0))
    assertResult(Set(Tuple(50.0,-50.0),Tuple(60.0,-60.0),Tuple(70.0,-70.0)))(fY(-60.0))


  }

}
