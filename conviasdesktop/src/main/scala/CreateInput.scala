import scala.reflect.io.{Directory, File, Path}

object CreateInput extends App {
  case class InputSRT(durationPath: String, gpxPath: String, pathOutput: String, tramoName: String){
    override def toString: String = s"$durationPath;$gpxPath;$pathOutput;$tramoName"
  }
  val mapTramos = Map[String, String](
    "Tramo 1" -> "Tramo 1",
    "Tramo 2" -> "Tramo 2",
    "Tramo 3" -> "Tramo 3",
    "Tramo 4" -> "Tramo 4",
    "tramo 4sg" -> "Tramo 4",
    "05" -> "Tramo 5",
    "06" -> "Tramo 6",
    "07" -> "Tramo 7",
    "tramo 7-8-9" -> "Tramo 7,8,9",
  )
  def buildGPXDirectory(baseDirectory: Directory): Directory = Directory(baseDirectory.path+"/GPX")
  def buildSRTDirectory(baseDirectory: Directory): Directory = {
    val srt = Directory(baseDirectory.path+"/SRT")
    if(!srt.exists) {
      srt.createDirectory(failIfExists = true)
    }
    srt
  }
  def buildSRTFileOutput(baseDirectory: Directory, baseName: String): Path = {
    File(baseDirectory.path+"/SRT/"+baseName+".srt")
  }
  def buildInput(baseDirectory: Directory): List[InputSRT] = {
    val gpxDirectory = buildGPXDirectory(baseDirectory)
    val files = gpxDirectory.files.toList
    val baseNames = files.map(f => f.name.substring(0, f.name.indexOf("."))).distinct
    baseNames.flatMap{ baseName =>
      val tramoName = mapTramos(baseDirectory.name)
      val srtOutput = buildSRTFileOutput(baseDirectory, baseName)
      val res = for{
        fileDuration <- files.find(_.name.contains(s"$baseName.duration")).map(_.path)
        fileGPX <- files.find(_.name.contains(s"$baseName.gpx")).map(_.path)
      }yield{
        InputSRT(fileDuration, fileGPX, srtOutput.path, tramoName)
      }
      println(s"$baseName -> $res")
      res
    }
  }
  def untilGPXParent(baseDirectory: Directory): List[InputSRT] = {
    if(buildGPXDirectory(baseDirectory).exists){
      buildInput(baseDirectory)
    }else{
      baseDirectory.dirs.toList.flatMap(untilGPXParent)
    }
  }
  val inputs: List[InputSRT] = args.flatMap{ initialPath =>
    val root = Directory(initialPath)
    untilGPXParent(root)
  }.toList

  inputs.foreach(println)
}
