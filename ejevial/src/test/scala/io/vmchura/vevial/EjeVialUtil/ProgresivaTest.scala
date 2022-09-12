package io.vmchura.vevial.EjeVialUtil
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
class ProgresivaTest extends AnyFlatSpec with Matchers{

  behavior of "Progresiva"

  it should "build correctly from int" in {
    val s = new Progresiva(0)
    assertResult("  0+000")(s.show(withSpaces = true, withKmLeftPadding = 3))
  }
  it should "build correctly from str" in {
    val s = Progresiva("0+300")
    assertResult(Some("  0+300"))(s.map(_.show(withSpaces = true, withKmLeftPadding = 3)))
  }
}
