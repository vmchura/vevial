package io.vmchura.vevial.EjeVialBuilder

import io.vmchura.vevial.EjeVialUtil.Coordinates
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Codec
import scala.reflect.io.File

class KMLToEjeTest extends AnyFlatSpec with Matchers{

  behavior of "Creation and build of KMLToEje"

  it should "build correctly" in {
    val tramoPathKML = getClass.getClassLoader.getResource("Tramo1.kml").getPath
    val file = File(tramoPathKML)
    val xmlBuiler = new KMLToEje(file.reader(Codec("UTF-8")))
    val result = xmlBuiler.toEje
    result match {
      case Right(value) =>
        assertResult(4370)(value.elements.length)
        println(value.length)
        println(value.minProg)
        println(value.maxProg)
        val testPoints: List[(Double, Double, Double)] = List(
          (-72.9127993634,-13.6913537313, 400.0),
          (-72.6836302302,-14.083549374, 107900.0),
          (-72.57701672250001,-14.0707389289, 142400.0),
          (-72.2471233321,-14.1183959045, 219369.0)
        )
        testPoints.foreach{
          case (lat, lon, prog) =>
            val testProg = Coordinates(lat, lon).toPoint()
            val projection = value.projectPoint(testProg).map(value.calcProgresive)
            assert(projection.isDefined)
            projection.get should equal (prog +- 1e1)
        }



      case Left(errros) => println(errros)
    }
  }
}
