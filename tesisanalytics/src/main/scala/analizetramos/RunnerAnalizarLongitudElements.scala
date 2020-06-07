package analizetramos

import io.vmchura.vevial.EjeVialBuilder.LandXMLToEje
import RFunctionDefiner.RInitializer

import scala.io.Codec
import scala.reflect.io.File

object RunnerAnalizarLongitudElements {
  def main(args: Array[String]): Unit = {
    val fileXML = File("/home/vmchura/Documents/001.Projects/Vevial/ejevialview/src/test/resources/tramo123.xml")


    RInitializer.init()
    new LandXMLToEje(fileXML.reader(Codec("UTF-8"))).toEje match {
      case Right(value) =>
        val eje = value//.slice(95500,96750)
        val ce = new CalculatorLengthElements(eje)
        ce.calcAllLengthsAndSaveGraph("/home/vmchura/Documents/001.Projects/Vevial/tesisanalytics/src/main/scala/analizetramos/resultAll.png")
        ce.calcRectElementAndSaveGraph("/home/vmchura/Documents/001.Projects/Vevial/tesisanalytics/src/main/scala/analizetramos/resultLineal.png")
        ce.calcCircElementAndSaveGraph("/home/vmchura/Documents/001.Projects/Vevial/tesisanalytics/src/main/scala/analizetramos/resultCircular.png")
        ce.calcCircAngleElementAndSaveGraph("/home/vmchura/Documents/001.Projects/Vevial/tesisanalytics/src/main/scala/analizetramos/resultAngleCirc.png")
        ce.calcCircAngleLengthElementAndSaveGraph("/home/vmchura/Documents/001.Projects/Vevial/tesisanalytics/src/main/scala/analizetramos/resultAngleLengthCirc.png")
      case Left(value) => println(value)
    }
  }
}
