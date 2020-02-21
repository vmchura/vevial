package io.vmchura.vevial.IriReporter


import java.util.UUID

import io.vmchura.vevial.Calculator.IriCalculator
import io.vmchura.vevial.Calculator.models.singularitypoint.{SingularityPointRaw, SingularityPointReader}
import io.vmchura.vevial.EjeVialBuilder.LandXMLToEje
import io.vmchura.vevial.EjeVialUtil.Progresiva
import io.vmchura.vevial.relevamiento.RelevamientoIRI
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Codec
import scala.reflect.io.File

class ReporterTest extends AnyFlatSpec {

  behavior of "ReporterTest"

  it should "addData" in {

    val fileCSV1 = new java.io.File("/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/2019-02-04 09h12m01s Survey.csv")
    val fileCSV2 = new java.io.File("/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/2019-02-04 09h53m35s Survey.csv")
    val fileXML = File("/home/vmchura/Documents/001.Projects/vevial/ejevialview/src/test/resources/tramo123.xml")
    val ejeEither =  new LandXMLToEje(fileXML.reader(Codec("UTF-8"))).toEje

    assert(ejeEither.isRight)

    val eje = ejeEither match {
      case Right(value) => value
      case _ => null
    }


    val iriCalculator = new IriCalculator(eje,Progresiva(0),Progresiva(1e7.toInt))
    iriCalculator.includeFile(fileCSV1,"F1")
    iriCalculator.includeFile(fileCSV2,"F2")

    val file = new java.io.File("/home/vmchura/Documents/001.Projects/vevial/viricalc/src/test/resources/singularitypointsbasic.csv")
    val reader = new SingularityPointReader(file)
    val singularityPoints = reader.elements.zipWithIndex.map{case (l,indx) =>
      SingularityPointRaw(indx,l,eje)}.flatMap(_.toSingularityPoint(UUID.randomUUID()))

    val report = iriCalculator.process(1000,_ => 2.0,singularityPoints,"Tramo X","/home/vmchura/Pictures/HeaderCVSC.png")

    report.reporter.buildWorkBook() match {
      case Some(path) => println(s"path: ${path.toString}")
      case None => println("No path")
    }

  }


}
