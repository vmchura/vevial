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
    val findRange: ProgPointTangent => Option[(Int,Int)] = p => SubsequenceFinder.find(12,12)(rangeProg)(p.prog.toDouble)(_.prog)

    val averageWithClosestPoints: ProgPointTangent => Option[PointUnitaryVector] = p => {

      val res = findRange(p).flatMap{case (i,j) =>
        type PVMeta = (UPoint,UDirection)
        val dataMeta: Seq[PVMeta] = (i to j).flatMap(k => rangeProg(k).pointTangent.point.map(p => (p,rangeProg(k).pointTangent.tangent)))
        val u = dataMeta.reduceLeftOption[PVMeta]{case ((a,b),(x,y)) => (a |-| x, b |-| y)}
        u.map{ case (pd,dd) =>  PointUnitaryVector(pd.value,dd.value)}

      }
      res

    }

    //first try: segment each 100 m
    val sectionLength = 100d
    val sections: Seq[BuilderFixedPoints] = {
      val fleft: (List[BuilderFixedPoints], List[ProgPointTangent], Option[PointUnitaryVector], Progresiva) = relConProgTangent.foldLeft((List.empty[BuilderFixedPoints], List.empty[ProgPointTangent], Option.empty[PointUnitaryVector], Progresiva(Integer.MIN_VALUE))){
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
                averageWithClosestPoints(nextPoint).getOrElse(defaultValue)
              }
              val bb: BuilderFixedPoints = AxisFitProblem(start,end, currentPoints,prog.progresiva,nextPoint.prog) match {
                case Left(error) =>
                  println(s"ERROR $error")
                  BasicSectionBuilder(start, end,currentPoints)
                case Right(af) => af
              }
              (bb :: prevList, Nil, Some(end), Progresiva(nextPoint.prog))
            case (_,Some(_),_) =>
              (prevList, nextPoint :: currentPoints, Some(start), prog)
            case _ =>
              (prevList, currentPoints, Some(start), prog)
          }
      }

      val (list,elements,start,_) = fleft
      val res = if(elements.length > 1){
        val end = (elements.last.pointTangent.point,elements.last.pointTangent.tangent) match {
          case (Some(UPoint(p,_)),UDirection(d,_)) => Some(PointUnitaryVector(p,d))
          case _ => None
        }
        (start,end) match {
          case (Some(in),Some(out)) =>
            val bb: BuilderFixedPoints = AxisFitProblem(in,out, elements, elements.map(_.prog).min, elements.map(_.prog).max) match {
              case Left(error) =>
                println(s"ERROR $error")
                BasicSectionBuilder(in, out,elements)
              case Right(af) => af
            }
            (bb :: list).reverse

          case _ => list.reverse
        }

      }else{
        list.reverse
      }
      res
    }
    //BasicSectionBuilder.saveSequenceBasicSectionBuilder(sections.map(_.asInstanceOf[BasicSectionBuilder]),"/home/vmchura/Documents/datasectionbuilder.xml")

    loggger.debug(s"Size of sections ${sections.length}")

    new TConvertibleToEje {
      override protected def getSequenceElements: Either[Exception, EfficientSeqEjeElements] = {
        val inefficientEje = sections.foldLeft(Right(EmptySeqEjeElements()) :Either[Exception,TSeqEjeElementsBase]){case (prevSeq,newElement) =>
          prevSeq.flatMap{ prevEje =>
            newElement.elements match {
              case Left(errors) => Left(errors)
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
