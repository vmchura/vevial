package AutomaticBuilder.models

trait TIterativeImproving {
  val maxHeap = scala.collection.mutable.PriorityQueue.empty[ElementActionToImprove]

  final def addActionToImprove(ea: ElementActionToImprove): Unit = maxHeap += ea
  /**
    * changes the state previous to the next upgrade
    *   - ex. visually move the window to the next point
    *   - ex. automaticale, does not do anything
    */
  def locateUpgrade(element: TElementCanImprove): Unit

  /**
    * @return true if the apply was successful, false otherwise
    */
  def applyUpgrade(ea: ElementActionToImprove): Boolean



  /**
    * Check if element can receive actions to improve
    * @param element
    * @return
    */
  def isElementValid(element: TElementCanImprove): Boolean

  final def popNextUpgrade(): Option[ElementActionToImprove] = {
    var toRet = Option.empty[ElementActionToImprove]
    while(toRet.isEmpty && maxHeap.nonEmpty){
      val k = maxHeap.dequeue()
      if(isElementValid(k.elementCanImprove)){
        toRet = Some(k)
      }
    }
    toRet
  }

}
