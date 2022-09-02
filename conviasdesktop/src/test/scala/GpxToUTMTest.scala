import io.vmchura.vevial.EjeVialUtil.Progresiva
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source
import scala.reflect.io.File
import scala.xml.XML
class GpxToUTMTest extends AnyFlatSpec {
  "GpxToUTM" should "parseFiles and calculate UTM from files" in {
    val gpxSource = getClass.getClassLoader.getResource("001.gpx").getPath
    val tramoPath = getClass.getClassLoader.getResource("tramo1.xml").getPath
    val tramoFile = File(tramoPath)
    val gpxNode = XML.load(gpxSource)
    val res = GpxToUTM.parseFiles(gpxNode, tramoFile)
    res.map{ x =>
      x.foreach(println)
    }
    assert(res.isRight)
  }
  it should "fill progresiva" in {
    val initialData = List(Some(Progresiva(1)), Some(Progresiva(2)), None)
    val expectedresult = List(Progresiva(1), Progresiva(2), Progresiva(3))
    val actualResult = GpxToUTM.completeProgresiva(initialData)
    assertResult(expectedresult)(actualResult)
  }
  it should "parse complete" in {
    val gpxSource = getClass.getClassLoader.getResource("001.gpx").getPath
    val tramoPath = getClass.getClassLoader.getResource("tramo1.xml").getPath
    val tramoFile = File(tramoPath)
    val gpxNode = XML.load(gpxSource)
    val res = GpxToUTM.parse(gpxNode, tramoFile)

    assert(res.isRight)
    res.map(x => println(x.mkString(System.lineSeparator())))
  }
}
