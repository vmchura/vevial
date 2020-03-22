package io.vmchura.vevial.IriReporter

import org.joda.time.DateTime
import org.scalatest.flatspec.AnyFlatSpec

class ReportAppendixTest extends AnyFlatSpec {

  case class Image(path: String,progInt: Int,caption: String) extends TImage {

    override def lat: Double = Double.NaN

    override def lng: Double = Double.NaN

    override def prog(): Int = progInt


    override def date: DateTime = DateTime.now()
  }
  case class Respaldo(images: Seq[Image],progIni: Int, progFin: Int) extends TRespaldoByImages

  behavior of "Creation of appendix"
  it should "Generate images fine" in {
    val pathImages = Array("/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/0001_TRAMO_III_017+717.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/0001_TRAMO_III_017+783.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/0001_TRAMO_III_017+910.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/0001_TRAMO_III_018+168.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/0003_TRAMO_III_018+758.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/0003_TRAMO_III_018+792.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/0007_TRAMO_III_018+928.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/0007_TRAMO_III_018+977.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/0009_TRAMO_III_018+684.jpg",
    "/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/0009_TRAMO_III_018+702.jpg")


    val r0 = Respaldo(Seq(
      Image(pathImages(0),17717, "Curvas y contra curvas"),
      Image(pathImages(1),17783,"Curvas y contra curvas"),
      Image(pathImages(2),17910,"Curvas y contra curvas"),
      Image(pathImages(3),18168,"Curvas y contra curvas"),
    ),17717,18168)
    val r1 = Respaldo(Seq(
      Image(pathImages(4),18758,"Desnivel en la superficie producto de fisuras en el borde"),
      Image(pathImages(5),18792,"Desnivel en la superficie producto de fisuras en el borde")
    ),18758,18792)
    val r2 = Respaldo(Seq(
      Image(pathImages(6),18928,"Curvas y contra curvas"),
      Image(pathImages(7),18977,"Curvas y contra curvas")
    ),18928,18977)
    val r3 = Respaldo(Seq(
      Image(pathImages(8),18684,"Curvas y contra curvas"),
      Image(pathImages(9),18702,"Curvas y contra curvas")
    ),18684,18702)
    val r = Seq(r0,r1,r2,r3)
    val report = "/home/vmchura/Pictures/HeaderRandom.png"
    println(ReportAppendix.buildAppendix(respaldo = r,report,"TRAMO PRUEBA"))


  }
}
