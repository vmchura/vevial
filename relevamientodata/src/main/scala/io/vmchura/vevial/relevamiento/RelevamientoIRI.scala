package io.vmchura.vevial.relevamiento
import java.io.{File, FileReader}

import io.vmchura.vevial.elementdata.{CrudeIRIData, IRIElementData, UPoint}
import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference

import scala.collection.mutable.ListBuffer


/**
  * Representa una secuencia de datos referidos al IRI
  * no necesariamente del Roughmeter III
  * @param elements
  */
class RelevamientoIRI(override val elements: Seq[IRIElementData],val header: List[Array[String]],val interval: Int) extends TSimpleRelevamiento[IRIElementData] {


  override def sliceBy(minX: Double, maxX: Double, minY: Double, maxY: Double): RelevamientoIRI = {
    new RelevamientoIRI(elements.filter{ e =>
      e.point.exists { case UPoint(value, _) =>
        minX <= value.x && value.x <= maxX &&
          minY <= value.y && value.y <= maxY
      }
    },

      header :+ Array(s"Solo considerando puntos en el rango Este-Oeste: [$minX:$maxX] Norte-Sur: [$minY:$maxY]"), interval)
  }
}

object RelevamientoIRI{
  def apply(elements: Seq[IRIElementData],header: List[Array[String]], interval: Int): RelevamientoIRI = new RelevamientoIRI(elements,header,interval)

  def apply(source: File): RelevamientoIRI = {

    import scala.collection.JavaConverters._

    val reader = new CsvListReader(new FileReader(source),CsvPreference.STANDARD_PREFERENCE)
    val registersBuffer = ListBuffer.empty[String]
    val headerBuffer = ListBuffer.empty[Array[String]]
    var continueRead = true
    var indx = 0
    while(continueRead){
      val lineRead = reader.read()
      if(lineRead!=null){
        val values=lineRead.asScala.toArray
        if(indx>=25){
          registersBuffer += values.mkString(",")
        }else{
          headerBuffer += values.takeWhile(r => r!=null)
        }

      }else{
        continueRead = false
      }
      indx = indx+1
    }


    //val (header, registers) = lines.getLines().toArray.splitAt(25)
    val registers = registersBuffer.toArray

    val header = headerBuffer.toList
    val interval: Int = header.find(_.exists(_.contains("Processing interval:"))) match {
      case Some(line) =>
        val indxPI = line.indexWhere(_.contains("Processing interval:"))
        val digitsOnly = line(indxPI+1).filter(_.isDigit)
        if(digitsOnly.isEmpty)
          throw new IllegalArgumentException("Incorrecto archivo, no es un .CSV válido/Not defined interval number")
        else
          digitsOnly.toInt
      case None =>
        throw new IllegalArgumentException("Incorrecto archivo, no es un .CSV válido/Not defined interval line")
    }
    val dataElements = registers.map(line => IRIElementData(new CrudeIRIData(line)))
    val n = dataElements.length
    dataElements.zipWithIndex.foreach {
      case (iriElement, indx) =>

        if (indx > 0) dataElements.update(indx, iriElement.withPrevElement(dataElements(indx - 1)))
        if (indx + 1 < n) dataElements.update(indx, iriElement.withNextElement(dataElements(indx + 1)))

    }
    //lines.close()

    new RelevamientoIRI(dataElements, header, interval)


  }
}

