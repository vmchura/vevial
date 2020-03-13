package tiles

import scala.collection.mutable

class IHT(sizeval: Int) {
  val size = sizeval
  var overfullCount = 0
  val dictionary = mutable.Map.empty[Any,Int]

  override def toString: String =
    s"""
      |Collision table:
      |size: ${size}
      |overfullCount: ${overfullCount}
      |dictionary: ${dictionary.size} items
      |""".stripMargin

  def count(): Int = dictionary.size
  def fullp(): Boolean = dictionary.size >= size
  def getindex(obj: Any): Int = {

    dictionary.get(obj) match {
      case Some(res) => res
      case None =>
        val cnt = count()
        if(cnt >= size){
          if(overfullCount == 0){
            println("IHT full, starting to allow collisions")
          }
          overfullCount += 1
          obj.hashCode() % size
        }else{
          dictionary += obj -> cnt
          cnt

        }
    }
  }

  def getindexReadOnly(obj: Any): Option[Int] = dictionary.get(obj)

}
