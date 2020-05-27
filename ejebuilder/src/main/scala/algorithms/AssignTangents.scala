package algorithms

import java.io.File

import com.typesafe.scalalogging.Logger
import io.vmchura.vevial.EjeVialBuilder.TConvertibleToEje
import io.vmchura.vevial.EjeVialUtil.Progresiva
import io.vmchura.vevial.PlanarGeometric.BasicEje.{EfficientSeqEjeElements, EmptySeqEjeElements, SubsequenceFinder, TSeqEjeElementsBase}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TDirection.Direction
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.PointUnitaryVector
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.TEfficientSeqEjeElementsProgresiva
import io.vmchura.vevial.PlanarGeometric.RestrictiveEje.ProgresivePoint
import io.vmchura.vevial.elementdata.{IRIElementData, UDirection, UPoint}
import io.vmchura.vevial.models.RelevamientoIRIProgresivas
import io.vmchura.vevial.relevamiento.RelevamientoIRI
import models.PointAlone

object AssignTangents {
  private val loggger = Logger("AssignTangents")

  /**
    *
    * @param ref:     Eje Referencial
    * @param files:   Data
    * @return EjeMejorado
    */
  def calcEjeWithBasicMath(ref: TEfficientSeqEjeElementsProgresiva, files: Seq[File]): TConvertibleToEje = {
    /**
      * 1: file => Relevamiento
      * 2: Relevamiento => RelevamientoConProgresiva
      * 3: RelevamientoConProgresiva => Seq elements + Tangent
      *
      * secction and improve each section
      *
      *
      */

    //1: file => Relevamiento
    val relevamientos = files.map(file => RelevamientoIRI(file,d => IRIElementData(d)))

    //2: Relevamiento => RelevamientoConProgresiva
    val relConProgresiva: Seq[RelevamientoIRIProgresivas[IRIElementData]] = relevamientos.map(r => new RelevamientoIRIProgresivas("a",r,ref,Progresiva(Integer.MIN_VALUE),Progresiva(Integer.MAX_VALUE)))

    //3: Relevamiento con tangentes
    val relConProgTangent = relConProgresiva.flatMap{ rel =>
      val pointsAlone = rel.elements.map{ r => ProgPointTangent(r.progresiva, PointAlone(r.iriElementData.point))}
      val pointsWithNext = pointsAlone.zip(pointsAlone.tail).map{case (ProgPointTangent(p,a),ProgPointTangent(_,b)) => ProgPointTangent(p,a.withNext(b))}
      val pointsWithPrev = pointsWithNext.zip(pointsWithNext.tail).map{case (ProgPointTangent(_,a),ProgPointTangent(p,b)) => ProgPointTangent(p,b.withPrev(a))}
      if(!rel.isForward){
        pointsWithPrev.map(r => r.copy(pointTangent = r.pointTangent.reverse()))
      }else{
        pointsWithPrev
      }
    }.sortBy(_.prog).toList

    val rangeProg = relConProgTangent.toArray
    val findRange: ProgPointTangent => Option[(Int,Int)] = p => SubsequenceFinder.find(40,40)(rangeProg)(p.prog.toDouble)(_.prog)

    val findClosestByProgressive: ProgPointTangent => Option[ProgPointTangent] = p => {

      val res = findRange(p).flatMap{case (i,j) =>
        loggger.debug(s"range: $i -> $j")
        (i to j).filter(k =>
          (for{
            p0 <- p.pointTangent.point
            p1 <- rangeProg(k).pointTangent.point
          }yield {
            (!(p0.value-p1.value)) > 1e-5
          }).getOrElse(false)
        ).minByOption(k => (for{
          p0 <- p.pointTangent.point
          p1 <- rangeProg(k).pointTangent.point
        }yield {
          !(p0.value-p1.value)
        }).getOrElse(100d))}

      (for{
        i <- res
        p0 <- p.pointTangent.point
        pi <- rangeProg(i).pointTangent.point
      }yield{
        if((!(p0.value - pi.value)) < 11d){
          loggger.debug(s"Range found at: $i")
          Some(rangeProg(i))
        }else{
          None
        }
      }).flatten


    }

    //first try: segment each 100 m
    val sectionLength = 40d
    val sections: Seq[BuilderFixedPoints] = {
      val (list,elements,start,_) = relConProgTangent.foldLeft((List.empty[BuilderFixedPoints], List.empty[ProgPointTangent], Option.empty[PointUnitaryVector], Progresiva(Integer.MIN_VALUE))){
        case ((prevList, currentPoints, None, prog), nextPoint) =>
          (nextPoint.pointTangent.point,nextPoint.pointTangent.tangent.value) match {
            case (Some(UPoint(p,_)),d: Direction) =>
              val start = Some(PointUnitaryVector(p,d))
                (prevList,nextPoint :: currentPoints, start, Progresiva(nextPoint.prog))
            case _ =>
              (prevList,currentPoints, None, prog)
          }
        case ((prevList, currentPoints, Some(start), prog), nextPoint) =>
          (nextPoint.prog - prog.progresiva, nextPoint.pointTangent.point, nextPoint.pointTangent.tangent.value) match {
            case (distanceToStart,Some(UPoint(point, _)),d: Direction) if distanceToStart > sectionLength =>


              val end: PointUnitaryVector = {
                val defaultValue = PointUnitaryVector(point,d)
                findClosestByProgressive(nextPoint).fold(defaultValue)(ppt => {
                  val mediaPoint = for{
                    p0 <- nextPoint.pointTangent.point
                    p1 <- ppt.pointTangent.point
                  }yield{
                    p0 |-| p1
                  }

                  val mediaTangent = nextPoint.pointTangent.tangent |-| ppt.pointTangent.tangent
                  mediaPoint.fold(defaultValue)(q => PointUnitaryVector(q.value,mediaTangent.value))
                })


              }
              val bb = BasicSectionBuilder(start,end, currentPoints)
              (bb :: prevList, Nil, Some(end), Progresiva(nextPoint.prog))
            case (_,Some(_),_) =>
              (prevList, nextPoint :: currentPoints, Some(start), prog)
            case _ =>
              (prevList, currentPoints, Some(start), prog)
          }
      }

      val res = if(elements.length > 1){
        val end = (elements.last.pointTangent.point,elements.last.pointTangent.tangent) match {
          case (Some(UPoint(p,_)),UDirection(d,_)) => Some(PointUnitaryVector(p,d))
          case _ => None
        }
        (start,end) match {
          case (Some(in),Some(out)) =>
            val bb = BasicSectionBuilder(in,out, elements)
            (bb :: list).reverse

          case _ => list.reverse
        }

      }else{
        list.reverse
      }
      res
    }

    loggger.debug(s"Size of sections ${sections.length}")

    new TConvertibleToEje {
      override protected def getSequenceElements: Either[Exception, EfficientSeqEjeElements] = {
        val inefficientEje = sections.foldLeft(Right(EmptySeqEjeElements()) :Either[Exception,TSeqEjeElementsBase]){case (prevSeq,newElement) =>
          prevSeq.flatMap{ prevEje =>
            newElement.elements match {
              case Left(errors) => Left(errors.headOption.getOrElse(new IllegalStateException("List of error with no error")))
              case Right(bbp) =>Right(prevEje.append(bbp))
            }
          }
        }

        inefficientEje.map(eje => EfficientSeqEjeElements(eje))


      }

      override protected def getSequenceProgresivePoint: Iterable[ProgresivePoint] = {
        sections.headOption match {
          case Some(bb) =>List(new ProgresivePoint(bb.in.point,0d))
          case None => Nil
        }

      }
    }

  }
}
