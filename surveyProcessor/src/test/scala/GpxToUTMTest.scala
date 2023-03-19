import io.vmchura.vevial.EjeVialBuilder.LandXMLToEje
import io.vmchura.vevial.EjeVialUtil.Progresiva
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{Codec, Source}
import scala.reflect.io.File
import scala.xml.XML
class GpxToUTMTest extends AnyFlatSpec {
  "GpxToUTM" should "parseFiles and calculate UTM from files" in {
    val gpxSource = getClass.getClassLoader.getResource("001.gpx").getPath
    val tramoPath = getClass.getClassLoader.getResource("tramo1.xml").getPath
    val tramoFile = File(tramoPath)
    val gpxNode = XML.load(gpxSource)
    val tramoEither = new LandXMLToEje(tramoFile.reader(Codec("UTF-8"))).toEje
    val res = GpxToUTM.parseFiles(gpxNode, tramoEither)
    assert(res.isRight)
  }
  it should "fill progresiva" in {
    val initialData = List(Some(Progresiva(1)), Some(Progresiva(2)), None)
    val expectedresult = List(Progresiva(1), Progresiva(2), Progresiva(3))
    val actualResult = GpxToUTM.completeProgresiva(initialData)
    assertResult(expectedresult)(actualResult)
  }
  it should "parse with removed mal formation" in {
    val gpxSource = getClass.getClassLoader.getResource("removed_mal_formation.gpx").getPath
    val tramoPath = getClass.getClassLoader.getResource("tramo2.xml").getPath
    val tramoFile = File(tramoPath)
    val tramoEither = new LandXMLToEje(tramoFile.reader(Codec("UTF-8"))).toEje
    val gpxNode = XML.load(gpxSource)
    val res = GpxToUTM.parse(gpxNode, tramoEither)

    assert(res.isRight)
  }
  it should "parse with bad formation" in {
    val gpxSource = getClass.getClassLoader.getResource("bad_formed.gpx").getPath
    val tramoPath = getClass.getClassLoader.getResource("tramo2.xml").getPath
    val tramoFile = File(tramoPath)
    val tramoEither = new LandXMLToEje(tramoFile.reader(Codec("UTF-8"))).toEje
    val gpxNode = XML.load(gpxSource)
    val res = GpxToUTM.parse(gpxNode, tramoEither)

    assert(res.isRight)
  }
}
