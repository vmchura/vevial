package io.vmchura.vevial.IriReporter

import org.joda.time.DateTime
import org.scalatest.flatspec.AnyFlatSpec

class ReportAppendixTest extends AnyFlatSpec {

  case class Image(path: String) extends TImage {

    override def lat: Double = 0.3

    override def lng: Double = 0.5

    override def prog(): Int = 5

    override def caption: String = path

    override def date: DateTime = DateTime.now()
  }
  case class Respaldo(images: Seq[Image]) extends TRespaldoByImages {
    override def progIni: Int = 15

    override def progFin: Int = 34
  }

  behavior of "Creation of appendix"
  it should "Generate images fine" in {
    val pathImages = Seq("/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/frame_926439019.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/frame_926439020.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/frame_926439021.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/frame_926439022.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/frame_926439023.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/frame_926439024.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/frame_926439025.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/frame_926439026.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/frame_926439027.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/frame_926439028.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/frame_926439029.jpg")


    val r0 = Respaldo(pathImages.take(5).map(Image))
    val r1 = Respaldo(pathImages.drop(5).map(Image))
    val r = Seq(r0,r1)
    val report = "/home/vmchura/Pictures/HeaderCVSC.png"
    println(ReportAppendix.buildAppendix(respaldo = r,report,"TRAMO ASD"))


  }
}
