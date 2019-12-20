package io.vmchura.vevial.relevamiento
import java.io.File

import io.vmchura.vevial.elementdata.{CrudeIRIData, IRIElementData, UPoint}


/**
  * Representa una secuencia de datos referidos al IRI
  * no necesariamente del Roughmeter III
  * @param elements
  */
class RelevamientoIRI(override val elements: Seq[IRIElementData],val header: List[String],val interval: Int) extends TSimpleRelevamiento[IRIElementData] {

  override def sliceBy(minX: Double, maxX: Double, minY: Double, maxY: Double): RelevamientoIRI = {
    new RelevamientoIRI(elements.filter{ e =>
      e.point.exists { case UPoint(value, _) =>
        minX <= value.x && value.x <= maxX &&
          minY <= value.y && value.y <= maxY
      }
    },header :+ s"Solo considerando puntos en el rango Este-Oeste: [$minX:$maxX] Norte-Sur: [$minY:$maxY]", interval)
  }
}

object RelevamientoIRI{
  def apply(elements: Seq[IRIElementData],header: List[String], interval: Int): RelevamientoIRI = new RelevamientoIRI(elements,header,interval)

  def apply(source: File): RelevamientoIRI = {

      val lines = scala.io.Source.fromFile(source, "UTF-8")
      val (header, registers) = lines.getLines().toArray.splitAt(25)
      val interval: Int = header.find(_.contains("Processing interval:")) match {
        case Some(line) => {
          val digitsOnly = line.filter(_.isDigit)
          if(digitsOnly.isEmpty)
            throw new IllegalArgumentException("Incorrecto archivo, no es un .CSV válido/Not defined interval number")
          else
            digitsOnly.toInt
        }
        case None => throw new IllegalArgumentException("Incorrecto archivo, no es un .CSV válido/Not defined interval line")
      }

      val dataElements = registers.map(line => IRIElementData(new CrudeIRIData(line)))
      val n = dataElements.length
      dataElements.zipWithIndex.foreach {
        case (iriElement, indx) =>

          if (indx > 0) dataElements.update(indx, iriElement.withPrevElement(dataElements(indx - 1)))
          if (indx + 1 < n) dataElements.update(indx, iriElement.withNextElement(dataElements(indx + 1)))

      }
      lines.close()

      new RelevamientoIRI(dataElements, header.toList, interval)


  }
}

