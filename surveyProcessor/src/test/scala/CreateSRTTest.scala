import io.vmchura.vevial.EjeVialBuilder.LandXMLToEje
import io.vmchura.vevial.EjeVialUtil.Progresiva
import models.ProgresivaMilliseconds
import org.scalatest.flatspec.AnyFlatSpec

import java.time.ZonedDateTime
import scala.io.Codec
import scala.reflect.io.File
import scala.xml.XML

class CreateSRTTest extends AnyFlatSpec {
  val thereExistsVideoFiles = false
  "CreateSRT" should "millisecondsToHMS" in {
    assertResult("00:00:00,001")(CreateSRT.millisecondsToHMS(1))
    assertResult("00:00:00,100")(CreateSRT.millisecondsToHMS(100))
    assertResult("00:00:01,000")(CreateSRT.millisecondsToHMS(1000))
    assertResult("42:28:02,500")(CreateSRT.millisecondsToHMS(151200000+1680000+2000+500)) // 42:28:02.500
  }
  it should "generate subtitles with no errors" in {
    assume(thereExistsVideoFiles)
    val gpxSource = getClass.getClassLoader.getResource("001.gpx").getPath
    val tramoPath = getClass.getClassLoader.getResource("tramo1.xml").getPath
    val tramoFile = File(tramoPath)
    val gpxNode = XML.load(gpxSource)
    val pathVideo = "D:\\GH070632.MP4"
    val tramoEither = new LandXMLToEje(tramoFile.reader(Codec("UTF-8"))).toEje
    val res = CreateSRT.buildSubtitles(pathVideo, gpxNode, tramoEither, "tramoName")
    assert(res.isRight)
    val subtitles = res.getOrElse(List.empty[String])
  }

  it should "write subtitle file with no errors" in {
    assume(thereExistsVideoFiles)
    val gpxSource = getClass.getClassLoader.getResource("001.gpx").getPath
    val tramoPath = getClass.getClassLoader.getResource("tramo1.xml").getPath
    val tramoFile = File(tramoPath)
    val tramoEither = new LandXMLToEje(tramoFile.reader(Codec("UTF-8"))).toEje
    val gpxNode = XML.load(gpxSource)
    val pathVideo = "D:\\GH070632.MP4"
    CreateSRT.execute(pathVideo, gpxNode, tramoEither, "D:/GH070632.srt", "...")
  }
}
