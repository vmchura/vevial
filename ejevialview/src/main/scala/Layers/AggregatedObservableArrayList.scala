package Layers



import scala.reflect.ClassTag
import javafx.collections.{FXCollections, ListChangeListener, ObservableList}
import scalafx.Includes._
import scalafx.collections.ObservableBuffer

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class AggregatedObservableArrayList[T : ClassTag ](lists: IndexedSeq[ObservableBuffer[T]]) {
  private val sizes: Array[Int] = lists.map(_.size()).toArray
  private val aggregatedList: ObservableBuffer[T] = new ObservableBuffer[T]()
  private val addedElements: mutable.ListBuffer[T] = new mutable.ListBuffer[T]
  private val deletedElements: mutable.ListBuffer[T] = new mutable.ListBuffer[T]
  def getAddedAndDeleted(): (Seq[T],Seq[T]) = {
    val a = addedElements.toSeq
    val b = deletedElements.toSeq
    addedElements.clear()
    deletedElements.clear()
    (a,b)
  }
  lists.foreach(l => {
    aggregatedList.appendAll(l)
    addedElements.appendAll(l)
    appendList(l)
    }
  )
   def getAggregatedList(): ObservableBuffer[T] ={
     FXCollections.unmodifiableObservableList(aggregatedList)
   }


  private def appendList(list: ObservableBuffer[T]): Unit = {
    val listener = new InternalListModificationListener(list)
    list.addListener(listener)

  }

  private def getStartIndex(list: ObservableBuffer[T]): Int = {
    val indxOfList = lists.indexOf(list)
    sizes.take(indxOfList).sum
  }
  private def getEndIndex(list: ObservableBuffer[T],startIndex: Int): Int = {
    val indxOfList = lists.indexOf(list)
    sizes(indxOfList)+startIndex-1
  }




  private class InternalListModificationListener(list: ObservableBuffer[T]) extends ListChangeListener[T]{

    override def onChanged(change: ListChangeListener.Change[_ <: T]): Unit = {
      val changedList = change.getList
      val startIndex = getStartIndex(list)

      while(change.next()){

        val from = change.getFrom
        val to = change.getTo
        if(change.wasPermutated()){
          val copy =  aggregatedList.slice(startIndex+from,startIndex+to).toArray
          (from until to).foreach{ oldIndex =>
            val newIndex = change.getPermutation(oldIndex)
            copy(newIndex-from) = aggregatedList(startIndex+oldIndex)
          }
          aggregatedList.removeRange(startIndex+from,startIndex+to)
          aggregatedList.insertAll(startIndex+from,copy)

        }else{
          if(change.wasUpdated()){

          }else{
            if(change.wasRemoved()){


              val removed = change.getRemoved

              deletedElements.addAll(removed.asScala)
              aggregatedList.removeRange(startIndex+from,startIndex+from+removed.size())



              if(change.wasAdded()){
                val added = change.getAddedSubList
                aggregatedList.addAll(startIndex+from,added)

                addedElements.appendAll(added.asScala)
              }
            }else{
              if(change.wasAdded()){

                val added = change.getAddedSubList

                aggregatedList.addAll(startIndex+from,added)
                addedElements.appendAll(added.asScala)

              }else{
                if(change.wasReplaced()){
                    //already proceeded by (removed+added)
                }
              }
            }
          }
        }
      }

      val index = lists.indexOf(list)
      val newSize = changedList.size()
      sizes(index) = newSize
    }
  }
}
