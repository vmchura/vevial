package models
import io.vmchura.vevial.EjeVialUtil.Coordinates
import org.scalatest.flatspec.AnyFlatSpec

import java.io.ByteArrayInputStream
import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import scala.xml.XML
class GpxParserTest extends AnyFlatSpec {
  "GpxParserTest" should "parseFromNode from node" in {
    val initialString =
      """
        |<gpx version="1.0"
        | creator="ExifTool 12.42"
        | xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        | xmlns="http://www.topografix.com/GPX/1/0"
        | xsi:schemaLocation="http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd">
        |<trk>
        |<number>1</number>
        |<trkseg>
        |<trkpt lat="1" lon="2"></trkpt>
        |<trkpt lat="3" lon="4">
        |  <ele>4012.275</ele>
        |  <time>2022-08-28T22:53:03.620Z</time>
        |</trkpt>
        |<trkpt lat="5" lon="6">
        |  <ele>4011.448</ele>
        |  <time>2022-08-28T22:53:04.610Z</time>
        |</trkpt>
        |<trkpt>
        |  <ele>4011.448</ele>
        |  <time>2022-08-28T22:53:05.600Z</time>
        |</trkpt>
        |</trkseg>
        |</trk>
        |</gpx>
        |""".stripMargin
    val targetStream = new ByteArrayInputStream(initialString.getBytes())
    val node = XML.load(targetStream)

    val listRawGeodesic = GpxParser.parseFromNode(node)
    assertResult(List(
      Some(RawGeodesicTimeStamp(Some(Coordinates(1, 2)), None)),
      Some(RawGeodesicTimeStamp(Some(Coordinates(3, 4)), Some(ZonedDateTime.parse("2022-08-28T22:53:03.620Z")))),
      Some(RawGeodesicTimeStamp(Some(Coordinates(5, 6)), Some(ZonedDateTime.parse("2022-08-28T22:53:04.610Z")))),
      None
    ))(listRawGeodesic)
  }
  it should "completeTimeStamp beginning" in {
    val input = List(
      None,
      Some(ZonedDateTime.parse("2022-08-28T22:53:03.620Z")),
      Some(ZonedDateTime.parse("2022-08-28T22:53:04.610Z")),
    )
    val expectedResult = List(
      ZonedDateTime.parse("2022-08-28T22:53:02.630Z"),
      ZonedDateTime.parse("2022-08-28T22:53:03.620Z"),
      ZonedDateTime.parse("2022-08-28T22:53:04.610Z"),
    )
    val actualResult = AlgorithmFill.completeTimeStamp(input)
    assertResult(expectedResult)(actualResult)
  }
  it should "completeTimeStamp end" in {
    val input = List(
      Some(ZonedDateTime.parse("2022-08-28T22:53:03.620Z")),
      Some(ZonedDateTime.parse("2022-08-28T22:53:04.610Z")),
      None
    )
    val expectedResult = List(
      ZonedDateTime.parse("2022-08-28T22:53:03.620Z"),
      ZonedDateTime.parse("2022-08-28T22:53:04.610Z"),
      ZonedDateTime.parse("2022-08-28T22:53:05.600Z"),
    )
    val actualResult = AlgorithmFill.completeTimeStamp(input)
    assertResult(expectedResult)(actualResult)
  }
  it should "completeTimeStamp middle" in {
    val input = List(
      Some(ZonedDateTime.parse("2022-08-28T22:53:03.690Z")),
      None,
      None,
      Some(ZonedDateTime.parse("2022-08-28T22:53:06.660Z")),
      Some(ZonedDateTime.parse("2022-08-28T22:53:07.650Z")),

    )
    val expectedResult = List(
      ZonedDateTime.parse("2022-08-28T22:53:03.690Z"),
      ZonedDateTime.parse("2022-08-28T22:53:04.680Z"),
      ZonedDateTime.parse("2022-08-28T22:53:05.670Z"),
      ZonedDateTime.parse("2022-08-28T22:53:06.660Z"),
      ZonedDateTime.parse("2022-08-28T22:53:07.650Z")
    )
    val actualResult = AlgorithmFill.completeTimeStamp(input)
    assertResult(expectedResult)(actualResult)
  }
  it should "completeTimeStamp long middle absence" in {
    val now = ZonedDateTime.now()
    val init = List(now.plus(1, ChronoUnit.SECONDS),
      now.plus(2, ChronoUnit.SECONDS),
        now.plus(3, ChronoUnit.SECONDS)).map(Some.apply)
    val middle = List.fill(53)(Option.empty[ZonedDateTime])
    val end = List(now.plus(57, ChronoUnit.SECONDS),
      now.plus(58, ChronoUnit.SECONDS),
      now.plus(59, ChronoUnit.SECONDS)).map(Some.apply)

    val input = init ::: middle ::: end

    val actualResult = AlgorithmFill.completeTimeStamp(input)
    actualResult.zip(actualResult.tail).foreach{ case (prev, next) =>
      assertResult(ChronoUnit.SECONDS.between(init.head.get, init.tail.head.get))(ChronoUnit.SECONDS.between(prev, next))
    }
  }
  it should "completeTimeStamp irregularEndings" in {
    val now = ZonedDateTime.now()
    val input = List(
      None,
      Some(now.plus(1, ChronoUnit.SECONDS)),
      Some(now.plus(2, ChronoUnit.SECONDS)),
      None,
      None,
      Some(now.plus(6, ChronoUnit.SECONDS)),
      Some(now.plus(8, ChronoUnit.SECONDS)),
      None
    )


    val actualResult = AlgorithmFill.completeTimeStamp(input)
    actualResult.zip(input).foreach(println)
  }
  it should "parse complete from node" in {
    val initialString =
      """
        |<gpx version="1.0"
        | creator="ExifTool 12.42"
        | xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        | xmlns="http://www.topografix.com/GPX/1/0"
        | xsi:schemaLocation="http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd">
        |<trk>
        |<number>1</number>
        |<trkseg>
        |<trkpt lat="1" lon="2"></trkpt>
        |<trkpt lat="3" lon="4">
        |  <ele>4012.275</ele>
        |  <time>2022-08-28T22:53:03.620Z</time>
        |</trkpt>
        |<trkpt lat="5" lon="6">
        |  <ele>4011.448</ele>
        |  <time>2022-08-28T22:53:04.610Z</time>
        |</trkpt>
        |<trkpt>
        |  <ele>4011.448</ele>
        |  <time>2022-08-28T22:53:05.600Z</time>
        |</trkpt>
        |</trkseg>
        |</trk>
        |</gpx>
        |""".stripMargin
    val targetStream = new ByteArrayInputStream(initialString.getBytes())
    val node = XML.load(targetStream)
    val expectedResult = List(
      GeodesicTimeStamp(Some(Coordinates(1,2)), ZonedDateTime.parse("2022-08-28T22:53:02.630Z")),
      GeodesicTimeStamp(Some(Coordinates(3,4)), ZonedDateTime.parse("2022-08-28T22:53:03.620Z")),
      GeodesicTimeStamp(Some(Coordinates(5,6)), ZonedDateTime.parse("2022-08-28T22:53:04.610Z")),
      GeodesicTimeStamp(None, ZonedDateTime.parse("2022-08-28T22:53:05.600Z")),
    )
    val actualResult = GpxParser.parse(node)
    assertResult(expectedResult)(actualResult)
  }
}
