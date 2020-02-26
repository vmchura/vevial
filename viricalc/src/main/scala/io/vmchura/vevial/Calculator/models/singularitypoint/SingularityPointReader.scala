package io.vmchura.vevial.Calculator.models.singularitypoint

import java.io.FileReader
import java.util.UUID

import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

class SingularityPointReader(file: java.io.File) {

  val elements: Seq[ParsedRawString] = {

    val reader = new CsvListReader(new FileReader(file),CsvPreference.STANDARD_PREFERENCE)
    var continueRead: Boolean = true
    val buffer = ListBuffer.empty[ParsedRawString]
    var headerToRead: Boolean = true
    while( continueRead ){
      val readElements = reader.read()
      if(readElements!=null){
        if(!headerToRead)
          buffer += ParsedRawString(readElements.asScala.toArray.takeWhile(_ != null))
        else{
          headerToRead=false
        }
      }else{
        continueRead = false
      }

    }
    buffer.toList
  }

  def getSingularityPoints(eje: EfficientEjeProgresiva): Seq[SingularityPoint] = {
    elements.zipWithIndex.map{case (l,indx) => SingularityPointRaw(indx,l,eje)}.flatMap(_.toSingularityPoint(UUID.randomUUID()))
  }
}
