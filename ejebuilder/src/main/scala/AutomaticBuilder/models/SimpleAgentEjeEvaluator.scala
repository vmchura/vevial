package AutomaticBuilder.models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint

import scala.collection.mutable.ListBuffer

object SimpleAgentEjeEvaluator extends TAgentEjeEvaluator {
  override final def deliberateAnAction(observer: TObserver): ActionImproveEje = {
    val elementsObserved = observer.elementsAdded()
    if(elementsObserved.isEmpty){
      NoAction
    }else{
      val elementToUpdate = observer.elementObserved
      val binX = elementsObserved.length match {
        case i if i <10 => 4
        case i if i < 20 => 10
        case _ => 20
      }
      val binY = binX/2
      val wx = elementToUpdate.length/binX
      val wy = elementsObserved.map(_.distanceNormal.abs).max/(binY/2)
      def indxX(x: Double): Int = {
        (x/wx).toInt
      }

      def indxY(y: Double): Int = {
        binY/2 + (y.abs/wy).toInt
      }

      val grid = Array.fill(binX+1,binY+1)(ListBuffer.empty[TProjection])
      elementsObserved.foreach{ projection =>
        val i = indxX(projection.distanceOverElement)
        val j = indxY(projection.distanceNormal)
        grid(i)(j).append(projection)

      }
      val gOpt = grid.flatten.filter(_.length>1).sortBy{ h =>
        val m = h.length
        h.map(_.distanceOverElement).sum/m
      }.find{
        h =>
          val m = h.length
          val yprom = h.map(_.distanceNormal).sum/m
          if(m >4)
            yprom.abs > 1
          else
            yprom.abs > 4
      }
      gOpt.map{ g =>
        val n = g.length
        val xProm = g.map(_.distanceOverElement).sum/n
        val yProm = g.map(_.distanceNormal).sum/n

        SetPointAt(xProm,yProm)

      }.getOrElse(NoAction)





    }

  }
}
