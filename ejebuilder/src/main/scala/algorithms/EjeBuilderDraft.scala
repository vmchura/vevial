package algorithms

import io.vmchura.vevial.EjeVialBuilder.TConvertibleToEje
import io.vmchura.vevial.PlanarGeometric.BasicEje.{EfficientSeqEjeElements, EmptySeqEjeElements, NonEmptySeqEjeElements, TSeqEjeElementsBase}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TPoint}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{CircleSegment, RectSegment, TCircleSegment, TEjeElement}
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.{EfficientEjeProgresiva, TEfficientSeqEjeElementsProgresiva}
import io.vmchura.vevial.PlanarGeometric.RestrictiveEje
import io.vmchura.vevial.PlanarGeometric.RestrictiveEje.EjeEfficientWithRestrictions
import io.vmchura.vevial.elementdata.TElementData
import io.vmchura.vevial.relevamiento.TSimpleRelevamiento
import models.{GeoNode, LinearGraph, MutableEje, TEjeBuilder}

class EjeBuilderDraft[+A <: TSimpleRelevamiento[B],B <: TElementData[B]](val relevamientos: Seq[A]) {




  def buildEje(): MutableEje = {

    val nodeEje: Seq[LinearGraph[GeoNode]] = DiscreteRelevamiento.convertIntoDiscreteRelevamiento[A,B,GeoNode](relevamientos)

    val singleLinearEje = LinearGraph.mergeLinearGraphs(nodeEje)

    val eje: TSeqEjeElementsBase = EjeBuilderDraft.buildEjeBase(singleLinearEje.nodes.toList)
    val ans = eje match {
      case NonEmptySeqEjeElements(elements) => new MutableEje(elements)
      case _ => throw new IllegalArgumentException("Cant build a eje")
    }

    ans


  }


}
object EjeBuilderDraft {
  def buildEjeBase(nodes: List[GeoNode]): TSeqEjeElementsBase = {
    import LinearEquationsSolver.{buildCircleSegment,buildCircleTangent}
    var numTimes = Int.MaxValue
    def buildEje(nodes: List[TPoint], prevElement: Option[TEjeElement]): TSeqEjeElementsBase = {
      (nodes,prevElement) match {
        case (a :: p :: b :: q :: tail, None) =>
          val initialPartEje: TSeqEjeElementsBase = (for{
            apb <- buildCircleSegment(a,p,b)
            pbq <- buildCircleSegment(p,b,q)
          }yield{
            val b1 = apb.out
            val b2 = (b-pbq.centerPoint).direction <¬ (if(pbq.antiClockWise) 1 else 3)
            val bDirOut = (b1.direction + b2).direction
            buildCircleTangent(apb.in,PointUnitaryVector(b,bDirOut)).map{ tangent =>
              if(tangent.in.point ==? a){
                NonEmptySeqEjeElements(List(tangent))
              }else{
                NonEmptySeqEjeElements(List(RectSegment(a,tangent.in.point),tangent))
              }
            }.getOrElse(NonEmptySeqEjeElements(List(RectSegment(a,b))))
          }).getOrElse(NonEmptySeqEjeElements(List(RectSegment(a,b))))

          initialPartEje.append(buildEje(b :: q :: tail,Some(initialPartEje)))

        case (b :: q :: c :: tail, Some(prev)) =>
          val complementEje: TSeqEjeElementsBase = (for{
            bqc <- buildCircleSegment(b,q,c)
            tangent <- buildCircleTangent(prev.out,bqc.out)
          }yield{



            if(tangent.in.point ==? prev.out.point){
              NonEmptySeqEjeElements(List(tangent))
            }else{
              NonEmptySeqEjeElements(List(RectSegment(prev.out.point,tangent.in.point),tangent))
            }




          }).getOrElse(NonEmptySeqEjeElements(List(RectSegment(prev.out.point,c))))

          numTimes -= 1
          if(numTimes > 0)
            complementEje.append(buildEje(c :: tail,Some(complementEje)))
          else
            complementEje


        case _ => EmptySeqEjeElements()
      }
    }

    buildEje(nodes.map(_.center),None)
  }
}

