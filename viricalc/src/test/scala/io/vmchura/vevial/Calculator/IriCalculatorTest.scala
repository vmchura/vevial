package io.vmchura.vevial.Calculator


import io.vmchura.vevial.Calculator.models.singularitypoint.SingularityPointReader
import io.vmchura.vevial.EjeVialBuilder.LandXMLToEje
import io.vmchura.vevial.EjeVialUtil.Progresiva
import io.vmchura.vevial.elementdata.IRIElementData
import io.vmchura.vevial.EjeVialUtil.Progresiva
import io.vmchura.vevial.IriReporter.IriValueAfterProcess.ValueComplete
import io.vmchura.vevial.elementdata.IRIElementData
import io.vmchura.vevial.relevamiento.RelevamientoIRI
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Codec
import scala.reflect.io.File

class IriCalculatorTest extends AnyFlatSpec {

  behavior of "Iri Calculator"

  it should "build correctly" in {
    val fileCSV = new java.io.File("/home/vmchura/Documents/001.Projects/vevial/relevamientodata/src/test/resources/2019-03-05 14h36m22s Survey.csv")
    val fileXML = File("/home/vmchura/Documents/001.Projects/vevial/ejevialview/src/test/resources/tramo123.xml")
    val ejeEither =  new LandXMLToEje(fileXML.reader(Codec("UTF-8"))).toEje

    assert(ejeEither.isRight)

    val eje = ejeEither match {
      case Right(value) => value
      case _ => null
    }

    val relevamientoIRI = RelevamientoIRI(fileCSV,cd => IRIElementData(cd))
    val relevamientoIRI = RelevamientoIRI(fileCSV,cf => IRIElementData(cf))

    val iriCalculator = new IriCalculator(eje,Progresiva(0), Progresiva(1e7.toInt))

  }
  it should "delete progresives" in {
    val fileCSV0 = new java.io.File("/home/vmchura/Documents/003.CVSC/IRI/Auomated/2020-02-21 12h13m18s Survey T1 HDER.csv")
    val fileCSV1 = new java.io.File("/home/vmchura/Documents/003.CVSC/IRI/Auomated/2020-02-21 12h50m20s Survey T1 HIZQ.csv")
    val fileCSVSP = new java.io.File("/home/vmchura/Documents/002.DescargasFirefox/reportSingPoint9286148934393071799.csv")
    val fileXML = File("/home/vmchura/Documents/001.Projects/vevial/ejevialview/src/test/resources/tramo123.xml")
    val ejeEither =  new LandXMLToEje(fileXML.reader(Codec("UTF-8"))).toEje

    assert(ejeEither.isRight)

    val eje = ejeEither match {
      case Right(value) => value
      case _ => null
    }


    val iriCalculator = new IriCalculator(eje,Progresiva(0), Progresiva(1e7.toInt))
    iriCalculator.includeFile(fileCSV0,"D")
    iriCalculator.includeFile(fileCSV1,"I")
    val sp = new SingularityPointReader(fileCSVSP)
    //
    val process = iriCalculator.process(1000,_ => 0.5, sp.getSingularityPoints(eje),"Tramo I","/home/vmchura/Pictures/HeaderCVSC.png")

    val iriCalculator = new IriCalculator(eje,Progresiva(0),Progresiva(1e7.toInt))

    val r = process.reporter.carrilIzquierdoData.filter(a => a.progresiva >= 180 && a.progresiva <= 270).filter{

        case l: ValueComplete => true
        case _ => false

    }
    assert(r.forall(_.iriValue.get <= 10))
  }
}
