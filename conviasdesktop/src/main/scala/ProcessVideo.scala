//import io.vmchura.vevial.EjeVialUtil.Progresiva
//import models.ProgresivaMilliseconds
//import org.jcodec.api.FrameGrab
//import org.jcodec.api.awt.AWTSequenceEncoder
//import org.jcodec.common.io.NIOUtils
//import org.jcodec.common.model.{Picture, Rational}
//import org.jcodec.scale.AWTUtil
//
//import java.awt.image.ImageObserver
//import javax.imageio.ImageIO
//import java.awt.{Color, Font, Image}
//import java.io
//import java.text.SimpleDateFormat
//import java.time.{LocalDateTime, ZoneOffset, ZonedDateTime}
//import java.util.{Date, TimeZone}
//import scala.concurrent.Await
//import scala.reflect.io.File
//import scala.xml.Node
//
//object ProcessVideo {
//
//  def calculateProgresiva(prevProgresiva: ProgresivaMilliseconds, currentProgresiva: ProgresivaMilliseconds, time: Long): Progresiva = {
//      if(time < prevProgresiva.millisFromStart){
//        prevProgresiva.progresiva
//      }else{
//        if(time > currentProgresiva.millisFromStart){
//          currentProgresiva.progresiva
//        }else{
//          val aTime = time - prevProgresiva.millisFromStart
//          val bProgresiva = currentProgresiva.progresiva.progresiva - prevProgresiva.progresiva.progresiva
//          val bTime = currentProgresiva.millisFromStart - prevProgresiva.millisFromStart
//          if(bTime == 0){
//            Progresiva(prevProgresiva.progresiva.progresiva)
//          }else {
//            val aProgresiva = aTime * bProgresiva / bTime
//            Progresiva(prevProgresiva.progresiva.progresiva + aProgresiva.toInt)
//          }
//        }
//      }
//  }
//
//  def findProgresiva(progresivas: List[ProgresivaMilliseconds], lastProgresiva: ProgresivaMilliseconds, time: Long): (Progresiva, List[ProgresivaMilliseconds], ProgresivaMilliseconds) = {
//    progresivas match {
//      case Nil => (lastProgresiva.progresiva, progresivas, lastProgresiva)
//      case current :: tail => if(time > current.millisFromStart){
//        findProgresiva(tail, current, time)
//      }else{
//        (calculateProgresiva(lastProgresiva, current, time), progresivas, lastProgresiva)
//      }
//    }
//  }
//  val imageObserver = new ImageObserver {
//    override def imageUpdate(img: Image, infoflags: Int, x: Int, y: Int, width: Int, height: Int): Boolean = false
//  }
//  def execute(pathVideo: String, gpxXML: Node, outPath: String, pathLogo: String): Either[Exception, Boolean] = {
//    GpxToUTM.parse(gpxXML, ???).map{ progresivasTimeStamp =>
//      val logoImage = ImageIO.read(new io.File(pathLogo))
//      val file = new java.io.File(pathVideo)
//      val out = NIOUtils.writableFileChannel(outPath)
//      val grab = FrameGrab.createFrameGrab(NIOUtils.readableChannel(file))
//      val totalDuration = grab.getVideoTrack.getMeta.getTotalDuration.toInt*1000
//      val totalFrames = grab.getVideoTrack.getMeta.getTotalFrames
//      val encoder = new AWTSequenceEncoder(out, Rational.R(30, 1))
//      var picture: Picture = grab.getNativeFrame
//      var onlyFewSeconds = 2*30
//      var currentList = progresivasTimeStamp
//      var lastProgresiva = ProgresivaMilliseconds(Progresiva(0), -1L, ZonedDateTime.now())
//      var i = 0
//      val dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
//      dateFormatter.setTimeZone(TimeZone.getTimeZone("GMT-5"))
//      while((picture != null)
//        //& (onlyFewSeconds > 0)
//      ){
//        val secondCalculated = (i *1L* totalDuration) / totalFrames
//        val (progresivaToWrite, newList, newLast) = findProgresiva(currentList, lastProgresiva, secondCalculated)
//        currentList = newList
//        lastProgresiva = newLast
//        //println(s"for $secondCalculated => $progresivaToWrite, $newLast")
//        val bufferedImage = AWTUtil.toBufferedImage(picture)
//
//        val g = bufferedImage.getGraphics
//        val h = picture.getHeight
//
//        g.drawImage(logoImage, 0, 0, imageObserver)
//        g.setFont(new Font("FreeMono", Font.PLAIN, 72))
//        g.setColor(Color.YELLOW)
//        g.drawString("Tramo\t\t 1", 72, h-72*3)
//
//        val utcTime = dateFormatter.format(Date.from(lastProgresiva.timeZoned.toInstant))
//        g.drawString(s"Tiempo aproximado\t: ${utcTime}", 72, h-72*2)
//        g.drawString(s"Progresiva aproximada\t ${progresivaToWrite.show(withSpaces = true, withKmLeftPadding = 3)}", 72, h-72)
//        g.dispose()
//        encoder.encodeImage(bufferedImage)
//        picture = grab.getNativeFrame
//        onlyFewSeconds = onlyFewSeconds - 1
//        i = i + 1
//      }
//      encoder.finish()
//      true
//    }
//  }
//}
