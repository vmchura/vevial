package io.vmchura.vevial.Calculator.models

import java.io.FileReader

import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference
import scala.collection.JavaConverters._

import scala.collection.mutable.ListBuffer

class FactorCorreccionCalculator(file: java.io.File) {
  private val reader = new CsvListReader(new FileReader(file),CsvPreference.STANDARD_PREFERENCE)
  private  var continueRead: Boolean = true
  private  case class TramoHomogeneo(progIni: Int, progFin: Int, factorCorreccion: Double)
  private  val buffer = ListBuffer.empty[TramoHomogeneo]
  private  var headerToRead: Boolean = true
  while( continueRead ){
    val readElements = reader.read()
    if(readElements!=null){
      if(!headerToRead) {
        val items = readElements.asScala.toArray.takeWhile(_ != null)
        if (items.length!=3) {
          throw new IllegalArgumentException("El rango está mal definido")
        } else {
          buffer += TramoHomogeneo(items(0).toInt,items(1).toInt,items(2).toDouble)
        }
      }
      else{
        headerToRead=false
      }
    }else{
      continueRead = false
    }

  }
  private val ranges = buffer.toList.sortBy(_.progIni)
  val calcFactorCorrection: Int => Double = (prog: Int) => {
    ranges.find(r => r.progIni <= prog && prog<r.progFin).map(_.factorCorreccion).getOrElse(0.0)
  }

}
