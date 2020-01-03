package io.vmchura.vevial.Calculator.models.singularitypoint

import java.io.FileReader

import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

class SingularityPointReader(file: java.io.File) {

  val elements: Seq[ParsedRawString] = {
    val reader = new CsvListReader(new FileReader(file),CsvPreference.STANDARD_PREFERENCE)
    var continueRead: Boolean = true
    val buffer = ListBuffer.empty[ParsedRawString]
    while( continueRead ){
      val readElements = reader.read()
      if(readElements!=null){
        buffer += ParsedRawString(readElements.asScala.toArray.takeWhile(_ != null))
      }else{
        continueRead = false
      }

    }
    buffer
  }
}
