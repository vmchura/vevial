package io.vmchura.vevial.Calculator


import io.vmchura.vevial.Calculator.models._
import io.vmchura.vevial.Calculator.models.singularitypoint.SingularityPoint
import io.vmchura.vevial.EjeVialUtil.Progresiva
import io.vmchura.vevial.IriReporter.{IriValueAfterProcess, Reporter}
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import io.vmchura.vevial.elementdata.IRIElementData
import io.vmchura.vevial.relevamiento.RelevamientoIRI

import scala.collection.mutable.ListBuffer

class IriCalculator(ejeVial: EfficientEjeProgresiva, progFrom: Progresiva, progTo: Progresiva) {

  val relevamientosIRIBuffer: ListBuffer[RelevamientoIRIProgresivas[IRIElementData]] = ListBuffer.empty[RelevamientoIRIProgresivas[IRIElementData]]

  /**
    *
    * @param file     file that contains iri values, csv
    * @param fileTag  a tag to use for the file, usually the filename
    * @return If it is included correctly
    */
  def includeFile(file: java.io.File,fileTag: String): Boolean = {
    try{
      val relevamientoIRI = RelevamientoIRI(file, cf => IRIElementData(cf))
      if(relevamientoIRI.elements.isEmpty)
        false
      else{
        relevamientosIRIBuffer += new RelevamientoIRIProgresivas(fileTag,relevamientoIRI,ejeVial,progFrom,progTo)
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
    * @param interval usually interval = 1000 (1km)
    * @return
    */
  def process(interval: Int, factorCorreccion: Int => Double,
              singularityPoints: Seq[SingularityPoint],tramoTag: String, pathHeader: String): IriCalculationResult = {
    if(relevamientosIRIBuffer.isEmpty)
      throw new IllegalStateException("No se ha incluido ningun archivo valido")

    val relevamientoIRI = relevamientosIRIBuffer.toList

    val minProg = Math.max(relevamientoIRI.map(_.minProg).min,0)
    val maxProg = relevamientoIRI.map(_.maxProg).max
    val reporter = new Reporter(relevamientoIRI,minProg,maxProg, interval,factorCorreccion,pathHeader,tramoTag)
    val (derRel,izqRel) = relevamientoIRI.partition(_.isForward)
    val singleDerRel: Seq[IRIElementDataProgresiva[IRIElementData]] = derRel.flatMap(_.elements)
    val singleIzqRel: Seq[IRIElementDataProgresiva[IRIElementData]] = izqRel.flatMap(_.elements)




    def processResultCarril(data: Seq[IRIElementDataProgresiva[IRIElementData]], isLeft: Boolean)(progInclusive: Int, progExclusive: Int): Option[ElementIriResultCarril] = {


      val singularityPointsToCheck = singularityPoints.filter{ sp =>
        val longitudSP = sp.progFin-sp.progIni
        val longitudRange = progExclusive-progInclusive
        val longitudTotal = Math.max(progExclusive,sp.progFin)-Math.min(progInclusive,sp.progIni)
        longitudTotal <= longitudSP + longitudRange
      }


      val elementsToUse = data.
        filter(e => progInclusive <= e.progresiva && e.progresiva < progExclusive)

      val (pointsToDiscard,pointsToEvaluate) = elementsToUse.
        partition(e => singularityPointsToCheck.exists(_.containsPoint(e.progresiva)))



      reporter.addData(pointsToDiscard.map{ e =>
        val description = singularityPointsToCheck.filter(_.containsPoint(e.progresiva)).map(_.description).mkString("/")
        IriValueAfterProcess(e.indx,e.fileTag,e.progresiva,Some(description))
      },isLeft)

      reporter.addData(pointsToEvaluate.filter(_.iriElementData.iriValue.isEmpty).map{ e =>
        IriValueAfterProcess(e.indx,e.fileTag,e.progresiva,Some("Sin lectura sensor"))
      },isLeft)

      reporter.addData(pointsToEvaluate.filterNot(_.iriElementData.iriValue.isEmpty).map{ e =>
        IriValueAfterProcess(e.indx,
          e.fileTag,e.progresiva,
          Some(e.iriElementData.iriValue.get),None)
      },isLeft)




      val elementsIRI = pointsToEvaluate.map(e => ElementIri(e.progresiva,e.progresiva,e.iriElementData.iriValue))

      ElementIriResultCarril(elementsIRI,factorCorreccion((progInclusive+progExclusive)/2))
    }
    def processCD(progInclusive: Int, progExclusive: Int): Option[ElementIriResultCarril] = processResultCarril(singleDerRel,isLeft = false)(progInclusive,progExclusive)
    def processCI(progInclusive: Int, progExclusive: Int): Option[ElementIriResultCarril] = processResultCarril(singleIzqRel,isLeft = true)(progInclusive,progExclusive)

    def process(progInclusive: Int, progExclusive: Int): ElementIriResult = {
      val resultDerecho = processCD(progInclusive,progExclusive)
      val resultIzquierdo = processCI(progInclusive,progExclusive)
      ElementIriResult(progInclusive,progExclusive,resultDerecho,resultIzquierdo)
    }
    val intervalResults = IriCalculator.genIntervals(minProg,maxProg,interval).map{ case (inc,excl) => process(inc,excl)}




    IriCalculationResult( new SortedElementIriResult(intervalResults),reporter)
  }



}

object IriCalculator{

  def genIntervals(minProg: Int, maxProg: Int, interval: Int): Seq[(Int,Int)] = {
    val startInterval = ((minProg + interval - 1) / interval) * interval
    val endInterval = (maxProg / interval) * interval

    val range = startInterval to endInterval by interval

    (if (minProg < startInterval) List((minProg, startInterval)) else Nil) ++
      range.zip(range.tail).toList ++
      (if (endInterval < maxProg) List((endInterval, maxProg)) else Nil)
  }



}
