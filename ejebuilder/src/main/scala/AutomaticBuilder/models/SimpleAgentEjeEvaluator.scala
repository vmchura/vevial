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
        case i if i <10 => 2
        case i if i < 20 => 5
        case _ => 10
      }
      val binY = binX*2
      val wx = elementToUpdate.length/binX
      val wy = elementsObserved.map(_.distanceNormal.abs).max/(binY/2)
      def indxX(x: Double): Int = {
        (x/wx).toInt
      }

      def indxY(y: Double): Int = {
        binX + (y.abs/wy).toInt
      }

      val grid = Array.fill(binX+1,binY+1)(ListBuffer.empty[TProjection])
      elementsObserved.foreach{ projection =>
        val i = indxX(projection.distanceOverElement)
        val j = indxY(projection.distanceNormal)
        grid(i)(j).append(projection)

      }
      val gOpt = grid.flatten.filter(_.length>1).minByOption{ h =>
        val m = h.length
        h.map(_.distanceOverElement).sum/m
      }
      gOpt.map{ g =>
        val n = g.length
        val xProm = g.map(_.distanceOverElement).sum/n
        val yProm = g.map(_.distanceNormal).sum/n
        if(yProm.abs < 5)
          NoAction
        else{
          SetPointAt(xProm,yProm)
        }
      }.getOrElse(NoAction)





    }

  }
}
