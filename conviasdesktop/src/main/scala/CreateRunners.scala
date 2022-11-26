import scala.io.Codec
import scala.reflect.io.{Directory, File}

object CreateRunners extends App{
  val rootDirectory = Directory(args(0))
  val rootAWS = "s3://prismaris-roads/JQBR/"
  val cutoff_date = "2022-10-31"
  case class Process(durationSeconds: Float, videoPath: String, srtPath: String, outputPath: String){
    def executions: String =
      s"""
        |aws s3 cp "$videoPath" input.MP4
        |aws s3 cp "$srtPath" input.srt
        |ffmpeg -y -i input.MP4 -i logo.png -filter_complex "[0]subtitles=input.srt:force_style='Alignment=1,OutlineColour=&H1000000&,PrimaryColour=&H000FFFF&,BackColour=&HFF&,BorderStyle=1,Outline=1,Shadow=0,Fontsize=14'[bg];[bg][1]overlay=0:0" -crf 28 output.MP4
        |aws s3 cp output.MP4 "$outputPath"
        |rm -f input.MP4
        |rm -f input.srt
        |rm -f output.MP4
        |""".stripMargin
  }
  val processes: List[Process] = {
    def findProcess(baseDirectory: Directory): List[Process] = {
      if(baseDirectory.name.equals("SRT")){
        val parent = baseDirectory.parent
        val srtFiles = baseDirectory.files.toList.filter(_.name.endsWith(".srt")).map{ srtFile =>
          val baseName = srtFile.name.replace(".srt","")

          val gpxDuration = File(parent.path+"/GPX/"+baseName+".duration").lines().toList.flatMap(_.toFloatOption).sum
          val awsBaseDirectory = rootAWS + parent.path.substring(baseDirectory.path.indexOf(cutoff_date)).replace('\\','/')
          val videoPath = awsBaseDirectory+"/"+baseName+".MP4"
          val srtPath = awsBaseDirectory+"/SRT/"+baseName+".srt"
          val outputPath = awsBaseDirectory+"/OUT/"+baseName+".MP4"
          Process(gpxDuration, videoPath, srtPath, outputPath)
        }

        srtFiles


      }else{
        if(baseDirectory.isDirectory){
          baseDirectory.dirs.toList.flatMap(findProcess)
        }else{
          Nil
        }
      }
    }

    findProcess(rootDirectory)
  }


  def timeToHours(processes: List[Process]): Float = {
    val sumTimeSeconds = processes.map(_.durationSeconds).sum
    val totalFrames = sumTimeSeconds * 30f
    val totalTimeProcessingSeconds = totalFrames / 2.1f
    val totalHours = totalTimeProcessingSeconds / (60 * 60)
    totalHours

  }

  println(f"Tiempo para procesar ${timeToHours(processes)}%.1f horas")
  val grouProcesses: List[List[Process]] = processes.foldLeft(List.empty[List[Process]]){
    case (Nil, process) => List(List(process))
    case (previous :: rest, process) => if(timeToHours(process :: previous) < 10f) {
      (process :: previous) :: rest
    } else{
      List(process) :: previous :: rest
    }
  }
  grouProcesses.zipWithIndex.foreach{ case (gp, i) =>
    gp.foreach{ p =>
      val baseName = p.videoPath.substring(p.videoPath.lastIndexOf("/")+1).replace(".MP4","")

      val file = File("D:/extremis/run_extremis_"+baseName+".sh")
      val writer = file.writer(append = false, Codec.UTF8)

      writer.write(
        """|#!/bin/bash
           |cd root
           |wget https://johnvansickle.com/ffmpeg/releases/ffmpeg-release-amd64-static.tar.xz
           |mkdir ffmpeg-release-amd64-static
           |tar -xf ffmpeg-release-amd64-static.tar.xz --strip-components=1 -C ffmpeg-release-amd64-static
           |rm -f ffmpeg-release-amd64-static.tar.xz
           |ln -s /root/ffmpeg-release-amd64-static/ffmpeg /usr/local/bin/ffmpeg
           |ln -s /root/ffmpeg-release-amd64-static/ffprobe /usr/local/bin/ffprobe
           |curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
           |unzip awscliv2.zip
           |./aws/install
          |yum -y install fontconfig
          |aws s3 cp s3://prismaris-roads/logo.png .
          |
          |""".stripMargin.replace("\r",""))

      writer.write("\n")
      writer.write(p.executions.replace("\r",""))
      writer.write("shutdown\n")
      writer.flush()
      writer.close()
    }


  }
}
