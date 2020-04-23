package AutomaticBuilder.models

object SimpleAgentEjeEvaluator extends TAgentEjeEvaluator {
  override final def deliberateAnAction(observer: TObserver): ActionImproveEje = {
    val elementsObserved = observer.elementsAdded()
    if(elementsObserved.isEmpty){
      NoAction
    }else{
      val numBins = 100
      val bins = (0 until numBins).map(_ => 0d).toArray
      val count = (0 until numBins).map(_ => 0).toArray
      val w: Double = observer.elementObserved.length/numBins

      def binIndx(lengthOverElement: Double): Int = {
        val groupsLeft = (lengthOverElement/w).toInt
        val i = groupsLeft
        Math.min(numBins-1,Math.max(0, i))
      }

      elementsObserved.foreach{ e =>
        val i = binIndx(e.distanceOverElement)
        (Math.max(0,i-0) to (Math.min(numBins-1,i+0))).foreach{ j =>
          val d = Math.max(1,Math.abs(j-i))
          bins(j) += (e.distanceNormal*e.distanceNormal*e.distanceNormal.sign)/d
          count(j) += 1
        }
      }

      bins.indices.foreach(i => if(count(i)> 0) bins(i) /= count(i))

      val indexMax = bins.zipWithIndex.maxBy(_._1.abs)._2


      val res = SetPointAt(indexMax*w+w/2d,Math.sqrt(bins(indexMax).abs)*bins(indexMax).sign)
/*
      println(elementsObserved.mkString(", "))
      println(bins.map(s => f"$s%.1f").mkString(" ,  "))
      println(res)

 */
      res
    }

  }
}
