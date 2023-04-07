package algorithms

import io.vmchura.vevial.EjeVialUtil.Progresiva
import models.ProgresivaMilliseconds

import java.time.{Instant, ZonedDateTime}
import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.collection.Searching._
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
  private def interpolate(p: ProgresivaMilliseconds, milliSecond: Int, q: ProgresivaMilliseconds): ProgresivaMilliseconds = {
    def interpolate[A](x0: A, xn: A, to_long: A => Long, from_long: Long => A): A = {
      def simple_interpolate(x0: Long, x: Long, xn: Long, r0: Long, rn: Long): Long = {
        val aTime = x - x0
        val bProgresiva = rn - r0
        val bTime = xn - x0
        if (bTime == 0) {
          r0
        } else {
          val aProgresiva = (aTime * bProgresiva * 1.0) / bTime
          (r0 + aProgresiva).toLong
        }
      }
      from_long(simple_interpolate(p.millisFromStart, milliSecond.toLong, q.millisFromStart, to_long(x0), to_long(xn)))
    }
    val progressDistance = interpolate[Progresiva](p.progresiva, q.progresiva, _.progresiva.toLong, prog => Progresiva(prog.toInt))
    val zonedTime = interpolate[ZonedDateTime](p.timeZoned, q.timeZoned, _.toInstant.toEpochMilli, millis => ZonedDateTime.ofInstant(Instant.ofEpochMilli(millis), p.timeZoned.getZone))
    ProgresivaMilliseconds(progressDistance, milliSecond, zonedTime)
  }

  def buildProgressDistanceByStepTime(progressDistanceTimeStamp: Array[ProgresivaMilliseconds],
                                      totalDuration: Duration,
                                      step: Duration): List[(Progresiva, Int, ZonedDateTime)] = {

    (0 until totalDuration.toMillis.toInt by step.toMillis.toInt).map{ milliSecond =>
      progressDistanceTimeStamp.search(ProgresivaMilliseconds(Progresiva(0), milliSecond, ZonedDateTime.now())) match {
        case Found(foundIndex) =>
          val ProgresivaMilliseconds(progressDistance, _, zonedDateTime) = progressDistanceTimeStamp(foundIndex)
          (progressDistance, milliSecond, zonedDateTime)
        case InsertionPoint(insertionPoint) =>
          val ProgresivaMilliseconds(progressDistance, _, zonedDateTime) = if(insertionPoint >= progressDistanceTimeStamp.length-1)
            progressDistanceTimeStamp.last
          else {
            if(insertionPoint == 0){
              progressDistanceTimeStamp.head
            }else{
              val previous_point = progressDistanceTimeStamp(insertionPoint)
              val next_point = progressDistanceTimeStamp(insertionPoint+1)
              interpolate(previous_point, milliSecond, next_point)
            }

          }

          (progressDistance, milliSecond, zonedDateTime)
      }
    }.toList
  }
}
