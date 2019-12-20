package models

/**
  * Evaluación de un conjunto de elements de IRI consecutivos
  * [progIni - progFin>
  * @param numElements: numElements has been evaluated
  * @param numElementsValidIRI: numElements has been evaluated
  * @param iriprom Optional, maybe no iri by failure of the machine
  * @param stdDev desviacion estándar de los puntos comprendidos en el segmento
  * @param factorCorrection factor de correcion a usar en el segmento
  *
  */
case class ElementIriResultCarril(numElements: Int, numElementsValidIRI: Int,
                                  iriprom: Option[Double], stdDev: Option[Double], factorCorrection: Double){
  val iric: Option[Double] = for{
    iri <- iriprom
    d <- stdDev
  } yield {

    iri + d*factorCorrection
  }
}


object ElementIriResultCarril{
  def apply(numElements: Int, numElementsValidIRI: Int,
            iriprom: Option[Double], stdDev: Option[Double], factorCorrection: Double): ElementIriResultCarril =
    new ElementIriResultCarril(numElements, numElementsValidIRI, iriprom, stdDev, factorCorrection)
  def apply(data: Seq[ElementIri], factorCorreccion: Double): Option[ElementIriResultCarril] = {
    if(data.isEmpty)
      None
    else {
      val numElements = data.length
      val validData = data.flatMap(_.iriValue)
      val numValidData = validData.length
      val (iriProm, std) = numValidData match {
        case 0 => (None, None)
        case 1 => (Some(validData.head), Some(0.0))
        case n =>
          val prom = validData.sum / n
          val std = Math.sqrt(validData.map(iri => (iri - prom) * (iri - prom)).sum / (n - 1))
          (Some(prom), Some(std))
      }
      Some(new ElementIriResultCarril( numElements, numValidData, iriProm, std, factorCorreccion))

    }
  }
}



