package io.vmchura.vevial.Calculator

import io.vmchura.vevial.Calculator.models._
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import io.vmchura.vevial.relevamiento.RelevamientoIRI

import scala.collection.mutable.ListBuffer

class IriCalculator(ejeVial: EfficientEjeProgresiva) {

  val relevamientosIRIBuffer: ListBuffer[RelevamientoIRIProgresivas] = ListBuffer.empty[RelevamientoIRIProgresivas]

  /**
    *
    * @param file
    * @return If it is included correctly
    */
  def includeFile(file: java.io.File): Boolean = {
    try{
      val relevamientoIRI = RelevamientoIRI(file)
      if(relevamientoIRI.elements.isEmpty)
        false
      else{
        relevamientosIRIBuffer += new RelevamientoIRIProgresivas(relevamientoIRI,ejeVial)
        true
      }
    }catch{
      case _: Exception => false
    }

  }

  /**
    * results are going to be split on intervals, each interval goes from [x*interval -> (x+1)*interval>
    * being x an integer
    * the intervals are inclusive at left, and exclusive at right
    * the first and last results could be less than an interval size
    * @param interval
    * @return
    */
  def process(interval: Int, factorCorreccion: Int => Double): IriCalculationResult = {
    if(relevamientosIRIBuffer.isEmpty)
      throw new IllegalStateException("No se ha incluido ningun archivo valido")
    val relevamientoIRI = relevamientosIRIBuffer.toList
    val minProg = relevamientoIRI.map(_.minProg).min
    val maxProg = relevamientoIRI.map(_.maxProg).max
    val (derRel,izqRel) = relevamientoIRI.partition(_.isForward)
    val singleDerRel: Seq[IRIElementDataProgresiva] = derRel.flatMap(_.elements)
    val singleIzqRel: Seq[IRIElementDataProgresiva] = izqRel.flatMap(_.elements)
    val startInterval = ((minProg+interval-1)/interval)*interval
    val endInterval = (maxProg /interval)*interval

    val range = startInterval to endInterval by interval

    def processResultCarril(data: Seq[IRIElementDataProgresiva])(progInclusive: Int, progExclusive: Int): Option[ElementIriResultCarril] = {
      val elementosCarril = data.filter(e => e.progresiva >= progInclusive && e.progresiva < progExclusive)
      val elementsIRI = elementosCarril.map(e => ElementIri(e.progresiva,e.progresiva,e.iriElementData.iriValue))
      ElementIriResultCarril(elementsIRI,factorCorreccion((progInclusive+progExclusive)/2))
    }
    def processCD(progInclusive: Int, progExclusive: Int): Option[ElementIriResultCarril] = processResultCarril(singleDerRel)(progInclusive,progExclusive)
    def processCI(progInclusive: Int, progExclusive: Int): Option[ElementIriResultCarril] = processResultCarril(singleIzqRel)(progInclusive,progExclusive)

    def process(progInclusive: Int, progExclusive: Int): ElementIriResult = {
      val resultDerecho = processCD(progInclusive,progExclusive)
      val resultIzquierdo = processCI(progInclusive,progExclusive)
      ElementIriResult(progInclusive,progExclusive,resultDerecho,resultIzquierdo)
    }
    val intervalResults = range.zip(range.tail).map{ case (inc,excl) => process(inc,excl)}

    val firstInterval: Seq[ElementIriResult] = if(minProg < startInterval){ List(process(minProg,startInterval)) }else Nil
    val lastInterval: Seq[ElementIriResult] = if(endInterval < maxProg){ List(process(endInterval,maxProg)) }else Nil


    IriCalculationResult( new SortedElementIriResult(intervalResults ++ firstInterval ++ lastInterval))
  }
}
