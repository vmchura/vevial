package io.vmchura.vevial.Calculator.models

import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import io.vmchura.vevial.relevamiento.RelevamientoIRI

class RelevamientoIRIProgresivas(relevamientoIRI: RelevamientoIRI, ejeVial: EfficientEjeProgresiva) {
  /**
    * Basic algorithm to assign progreivas.
    *   - First project point, and assign certainty of the projection
    *   - Find points that have corresponding: Projection - Sensor coherently, those points are certain
    *   - Until all elements have the certainty lower than a threshold update intervals
    *   - An interval are segments that the extrems points ARE certain
    *   - Just use te values of the sensor to update progresivas*
    */

  val elements: Seq[IRIElementDataProgresiva] = {
    val elementsWithOnlyProjection = relevamientoIRI.elements.zipWithIndex.map { case (e, indx) => {
      val projectionCertainty = for {
        p <- e.point
        q <- ejeVial.projectPoint(p.value)

      } yield {
        val prog = ejeVial.calcProgresive(q)
        val u = p.value-q
        (prog.toInt,u.magnitude*u.magnitude)
      }


      IRIElementDataOptProgresiva(e,indx,projectionCertainty.map(_._1),projectionCertainty.map(_._2).getOrElse(100.0))

    }}.toArray

    val indxCoeherent = (elementsWithOnlyProjection.zip(elementsWithOnlyProjection.tail).flatMap{ case (a,b) =>
      for{
        progA <- a.progresiva
        progB <- b.progresiva
      }yield{
        val deltaProg = Math.abs(progA-progB)
        val errorMargin = relevamientoIRI.interval*1.25
        if(deltaProg <= errorMargin && a.certainty<10 && b.certainty <10){
          //both point are coherent
          Seq(a.indx,b.indx)
        }else{
          Nil
        }
      }
    }).flatten.distinct.sorted

    if(indxCoeherent.length < 1)
      throw new IllegalStateException("No existe ningun punto coherente")


    val isForward =
      (for{
        pi <- elementsWithOnlyProjection(indxCoeherent.head).progresiva
        pf <- elementsWithOnlyProjection(indxCoeherent.last).progresiva
      }yield{
        pi < pf
      } )match {
        case Some(f) => f
        case None => throw new IllegalStateException("las progresivas de puntos coherentes no estan definidas?")
      }

    val interval = relevamientoIRI.interval
    def update(indx: Int, prog: Int): Unit = {
      elementsWithOnlyProjection.update(indx,elementsWithOnlyProjection(indx).copy(progresiva = Some(prog)))
    }
    def processFirstBlock(untilExclusive: Int): Unit = {
      val progEnd: Int = elementsWithOnlyProjection(untilExclusive).progresiva.getOrElse(throw new IllegalStateException("Progresiva de punto coherente no definido"))
      for(i <- 0 until untilExclusive){
        val newProg = progEnd-(untilExclusive-i)*interval*(if(isForward) 1 else -1)
        update(i,newProg)
      }
    }

    def processLastBlock(fromExclusive: Int): Unit = {
      val progStart: Int = elementsWithOnlyProjection(fromExclusive).progresiva.getOrElse(throw new IllegalStateException("Progresiva de punto coherente no definido"))
      for(i <- (fromExclusive+1) until elementsWithOnlyProjection.length){
        val newProg = progStart+(i-fromExclusive)*interval*(if(isForward) 1 else -1)
        update(i,newProg)
      }

    }

    def processBlock(fromExclusive: Int, untilExclusive: Int): Unit = {
      val progStart: Int = elementsWithOnlyProjection(fromExclusive).progresiva.getOrElse(throw new IllegalStateException("Progresiva de punto coherente no definido"))
      val progEnd: Int = elementsWithOnlyProjection(untilExclusive).progresiva.getOrElse(throw new IllegalStateException("Progresiva de punto coherente no definido"))
      val l = untilExclusive-fromExclusive
      val progGap = progEnd-progStart
      /**
        *  l -< progGap
        *  i -> prog
        *  prog = i*progGap/l
        */

      for(i <- (fromExclusive+1) until untilExclusive){
        val prog: Int = Math.round((i-fromExclusive)*progGap*1.0/l+progStart).toInt
        update(i,prog)
      }
    }

    processFirstBlock(indxCoeherent.head)
    processLastBlock(indxCoeherent.last)
    indxCoeherent.zip(indxCoeherent.tail).foreach{case (i,j) => {
      if(i < j-1){
        processBlock(i,j)
      }
    }}

    if(elementsWithOnlyProjection.forall(_.progresiva.isDefined))
      elementsWithOnlyProjection.map(e => IRIElementDataProgresiva(e.iriElementData,e.indx,e.progresiva.get))
    else throw new IllegalStateException("Not all elements tiene progresiva")
  }

  val (minProg,maxProg) = {
    val prog = elements.map(_.progresiva)
    (prog.min,prog.max)
  }

  val isForward = elements.head.progresiva < elements.last.progresiva


}
