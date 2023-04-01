package algorithms

import io.vmchura.vevial.EjeVialUtil.Progresiva
import models.ProgresivaMilliseconds

import java.time.ZonedDateTime
import scala.annotation.tailrec
import scala.concurrent.duration.Duration

object MarkSequentialByStepTime {

  private def calculateProgresiva(prevProgresiva: ProgresivaMilliseconds, currentProgresiva: ProgresivaMilliseconds, time: Long): Progresiva = {
    if (time < prevProgresiva.millisFromStart) {
      prevProgresiva.progresiva
    } else {
      if (time > currentProgresiva.millisFromStart) {
        currentProgresiva.progresiva
      } else {
        val aTime = time - prevProgresiva.millisFromStart
        val bProgresiva = currentProgresiva.progresiva.progresiva - prevProgresiva.progresiva.progresiva
        val bTime = currentProgresiva.millisFromStart - prevProgresiva.millisFromStart
        if (bTime == 0) {
          Progresiva(prevProgresiva.progresiva.progresiva)
        } else {
          val aProgresiva = aTime * bProgresiva / bTime
          Progresiva(prevProgresiva.progresiva.progresiva + aProgresiva.toInt)
        }
      }
    }
  }
  @tailrec
  private def findProgresiva(progresivas: List[ProgresivaMilliseconds], lastProgresiva: ProgresivaMilliseconds, time: Long): (Progresiva, List[ProgresivaMilliseconds], ProgresivaMilliseconds) = {
    progresivas match {
      case Nil => (lastProgresiva.progresiva, progresivas, lastProgresiva)
      case current :: tail => if (time > current.millisFromStart) {
        findProgresiva(tail, current, time)
      } else {
        (calculateProgresiva(lastProgresiva, current, time), progresivas, lastProgresiva)
      }
    }
  }

  def buildProgresiveByStepTime(progresivasTimeStamp: List[ProgresivaMilliseconds], totalDuration: Duration, step: Duration): List[(Progresiva, Int, ZonedDateTime)] = {
    var currentList = progresivasTimeStamp
    var lastProgresiva = ProgresivaMilliseconds(Progresiva(0), -1L, ZonedDateTime.now())
    (0 until totalDuration.toMillis.toInt by step.toMillis.toInt).zipWithIndex.map {case (milliSecond, _) =>
      val (progresivaToWrite, newList, newLast) = findProgresiva (currentList, lastProgresiva, milliSecond)
      currentList = newList
      lastProgresiva = newLast
      (progresivaToWrite, milliSecond, lastProgresiva.timeZoned)
    }.toList
  }
}
