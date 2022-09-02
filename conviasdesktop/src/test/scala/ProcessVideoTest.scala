import io.vmchura.vevial.EjeVialUtil.Progresiva
import models.ProgresivaMilliseconds
import org.scalatest.flatspec.AnyFlatSpec

import java.time.ZonedDateTime
import scala.reflect.io.File
import scala.xml.XML

class ProcessVideoTest extends AnyFlatSpec {
  "ProcessVideo" should "calculateProgresiva" in {
    val actualProg = ProcessVideo.calculateProgresiva(
      ProgresivaMilliseconds(Progresiva(100), 100L, ZonedDateTime.now()),
      ProgresivaMilliseconds(Progresiva(200), 200L, ZonedDateTime.now()),
      150
    ).progresiva
    val expectedProg = 150
    assertResult(expectedProg)(actualProg)
  }
  it should "calculateProgresiva extreme left" in {
    val actualProg = ProcessVideo.calculateProgresiva(
      ProgresivaMilliseconds(Progresiva(100), 10L, ZonedDateTime.now()),
      ProgresivaMilliseconds(Progresiva(200), 20L, ZonedDateTime.now()),
      11
    ).progresiva
    val expectedProg = 110
    assertResult(expectedProg)(actualProg)
  }

  it should "calculateProgresiva extreme right" in {
    val actualProg = ProcessVideo.calculateProgresiva(
      ProgresivaMilliseconds(Progresiva(100), 10L, ZonedDateTime.now()),
      ProgresivaMilliseconds(Progresiva(200), 20L, ZonedDateTime.now()),
      19
    ).progresiva
    val expectedProg = 190
    assertResult(expectedProg)(actualProg)
  }
  it should "findProgresiva simple" in {
    val (actualProg, restList, _) = ProcessVideo.findProgresiva(
      List(ProgresivaMilliseconds(Progresiva(100), 10L, ZonedDateTime.now())),
      ProgresivaMilliseconds(Progresiva(0), 5L, ZonedDateTime.now()),
      8
    )
    val expectedProg = 60
    assertResult(expectedProg)(actualProg.progresiva)
  }
  it should "findProgresiva dropping" in {
    val (actualProg, restList, _) = ProcessVideo.findProgresiva(
      List(
        ProgresivaMilliseconds(Progresiva(100), 10L, ZonedDateTime.now()),
        ProgresivaMilliseconds(Progresiva(200), 20L, ZonedDateTime.now()),
      ),
      ProgresivaMilliseconds(Progresiva(0), 5L, ZonedDateTime.now()),
      15
    )
    val expectedProg = 150
    assertResult(expectedProg)(actualProg.progresiva)
  }
  it should "create SimpleVideo" in {
    val gpxSource = getClass.getClassLoader.getResource("001.gpx").getPath
    val tramoPath = getClass.getClassLoader.getResource("tramo1.xml").getPath
    val tramoFile = File(tramoPath)
    val gpxNode = XML.load(gpxSource)
    val pathVideo = "D:\\GH070632.MP4"
    val res = ProcessVideo.execute(pathVideo, gpxNode, tramoFile, "D:\\salida.mp4", "D:\\image.jpeg")

  }
}
