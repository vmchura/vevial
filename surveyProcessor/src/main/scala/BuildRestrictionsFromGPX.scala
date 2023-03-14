import io.vmchura.vevial.EjeVialUtil.Coordinates
import io.vmchura.vevial.PlanarGeometric.RestrictiveEje.ProgresivePoint
import models.{GeodesicTimeStamp, GpxParser}

import scala.io.Codec
import scala.reflect.io.{Directory, File}
import scala.xml.XML
object BuildRestrictionsFromGPX  extends App{
  val gpxDirectory = Directory(args(0))
  val restrictionsCSV = File(args(1))

  def mp4ToGPX(mp4Name: String): String = {
    mp4Name.replace(".MP4",".gpx")
  }
  case class RestrictionByTime(timeVideo: Long, progressive: Int)
  val restrictionsRaw: Map[String, List[RestrictionByTime]] = restrictionsCSV.lines().map{ line =>
    val Array(fileName, time, progressive) = line.replace("\"","").split(",")
    fileName -> (time, progressive)
  }.toList.groupBy(_._1).map{
    case (k,v) => (mp4ToGPX(k), v.map{case (_, (timeStr, progStr)) => RestrictionByTime(timeStr.toLong, progStr.toInt)})
  }

  val restrictions: List[ProgresivePoint] = restrictionsRaw.flatMap{ case (keyFileName, restrictionsByTimeList) =>
    gpxDirectory.files.find(_.name.equals(keyFileName)).map{ gpxFile =>
      val gpxXML = XML.load(gpxFile.path)

      val result = GpxParser.findInitialTime(gpxXML).map{ initialTimeStamp => {
        val gpxGeodesics = GpxParser.parse(gpxXML)
        val geodesicOrdered = gpxGeodesics.distinctBy(_.timeStamp).toArray
        restrictionsByTimeList.flatMap { restrictionByTime =>
          val newTime = initialTimeStamp.plusNanos(restrictionByTime.timeVideo)
          val ip = geodesicOrdered.search(GeodesicTimeStamp(None, newTime)).insertionPoint
          geodesicOrdered(ip).geodesicCoordinatesOption.map { geodesicCoordinates =>
            new ProgresivePoint(geodesicCoordinates.toPoint(), restrictionByTime.progressive)
          }
        }
      }
      }

      result.getOrElse(Nil)
    }
  }.flatten.toList


  val outputCSV = File(args(2))
  val writer = outputCSV.writer(append = false, Codec.defaultCharsetCodec)
  restrictions.foreach{ progressivePoint =>
    writer.write(f"${progressivePoint.x}%10.10f,${progressivePoint.y}%10.10f,${progressivePoint.progresive}%10.10f")
    writer.write(System.lineSeparator())
  }
  writer.flush()
  writer.close()



}
