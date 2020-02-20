package io.vmchura.vevial.Calculator.algorithms

import io.vmchura.vevial.Calculator.models.IRIElementDataOptProgresiva
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import io.vmchura.vevial.elementdata.{DataWithPoint, TCrudeIRIData}

object AlgorithmAssignProgresivas {


    /**
      * Assigns to each element, a progresiva, does not do the final ordering
      *
      * * Basic algorithm to assign progreivas.
      * *   - First project point, and assign certainty of the projection
      * *   - Find points that have corresponding: Projection - Sensor coherently, those points are certain
      * *   - Until all elements have the certainty lower than a threshold update intervals
      * *   - An interval are segments that the extrems points ARE certain
      * *   - Just use te values of the sensor to update progresivas*
      **
      * @param data
      * @param interval
      * @param ejeVial
      * @return
      */
    def simpleAlgorithm[T <: DataWithPoint](data: Seq[T], interval: Int, ejeVial: EfficientEjeProgresiva): DataWithProgresivaSeq[(Int,T)] = {
        val elementsWithOnlyProjection = data.zipWithIndex.map { case (e, indx) => {
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
                val errorMargin = interval*1.25
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


        val isForwardResult =
            (for{
                pi <- elementsWithOnlyProjection(indxCoeherent.head).progresiva
                pf <- elementsWithOnlyProjection(indxCoeherent.last).progresiva
            }yield{
                pi < pf
            } )match {
                case Some(f) => f
                case None => throw new IllegalStateException("las progresivas de puntos coherentes no estan definidas?")
            }


        def update(indx: Int, prog: Int): Unit = {
            elementsWithOnlyProjection.update(indx,elementsWithOnlyProjection(indx).copy(progresiva = Some(prog)))
        }
        def processFirstBlock(untilExclusive: Int): Unit = {
            val progEnd: Int = elementsWithOnlyProjection(untilExclusive).progresiva.getOrElse(throw new IllegalStateException("Progresiva de punto coherente no definido"))
            for(i <- 0 until untilExclusive){
                val newProg = progEnd-(untilExclusive-i)*interval*(if(isForwardResult) 1 else -1)
                update(i,newProg)
            }
        }

        def processLastBlock(fromExclusive: Int): Unit = {
            val progStart: Int = elementsWithOnlyProjection(fromExclusive).progresiva.getOrElse(throw new IllegalStateException("Progresiva de punto coherente no definido"))
            for(i <- (fromExclusive+1) until elementsWithOnlyProjection.length){
                val newProg = progStart+(i-fromExclusive)*interval*(if(isForwardResult) 1 else -1)
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

        if(elementsWithOnlyProjection.forall(_.progresiva.isDefined)) {
            new DataWithProgresivaSeq[(Int,T)] {
                override val elements: Seq[(Int, T)] = elementsWithOnlyProjection.map(e => (e.progresiva.get,e.iriElementData))
                override def isForward: Boolean = isForwardResult
            }
           // elementsWithOnlyProjection.map(e => IRIElementDataProgresiva(e.iriElementData,e.indx,fileID,e.progresiva.get))
        } else throw new IllegalStateException("Not all elements tiene progresiva")
    }
}
