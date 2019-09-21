package Layers

import javafx.collections.{ ListChangeListener, ObservableList}
import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.delegate.SFXDelegate

import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag

class ObservableListDelegate[B <: Object,T <: SFXDelegate[B] : ClassTag](lists: IndexedSeq[ObservableBuffer[T]], observableList: ObservableList[B]) {

  lists.foreach(l => {
    observableList.appendAll(l.map(_.delegate))
    appendList(l)
    }
  )


  private def appendList(list: ObservableBuffer[T]): Unit = {
    val listener = new InternalListenerOfList(list)
    list.addListener(listener)

  }


  private class InternalListenerOfList(list: ObservableBuffer[T]) extends ListChangeListener[T]{

    override def onChanged(change: ListChangeListener.Change[_ <: T]): Unit = {

      while(change.next()){

        if(change.wasPermutated()){


        }else{
          if(change.wasUpdated()){

          }else{
            if(change.wasRemoved()){


              val removed = change.getRemoved
              observableList.removeAll(removed.asScala.map(_.delegate).asJava)


              if(change.wasAdded()){
                val added = change.getAddedSubList
                observableList.removeAll(added.asScala.map(_.delegate).asJava)
              }
            }else{
              if(change.wasAdded()){

                val added = change.getAddedSubList
                observableList.addAll(added.asScala.map(_.delegate).asJava)

              }else{
                if(change.wasReplaced()){
                    //already proceeded by (removed+added)
                }
              }
            }
          }
        }
      }

    }
  }
}
