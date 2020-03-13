package tiles

import scala.collection.mutable.ListBuffer

object Tiles {
  type IntIHT = Either[Int, IHT]
  def hashcoords(coordinates: Seq[Int],m: IntIHT): Int = {
    m match {
      case Left(mvalue) => coordinates.hashCode() % mvalue
      case Right(iht) => iht.getindex(coordinates)
    }
  }


  /**
    * returns num-tilings tile indices corresponding to the floats and ints
    * @param intIHT
    * @param numtilings
    * @param floats
    * @param ints
    * @return
    */

  def tiles(intIHT: IntIHT, numtilings: Int, floats: Seq[Float], ints: List[Int] = Nil): Seq[Int] = {
    val qfloats = floats.map{f => Math.floor(f*numtilings).toInt}
    (0 until numtilings).map{ tiling =>
      val tilingX2 = tiling*2

      val coords = qfloats.foldLeft((List(tiling),tiling)){
        case ((prevList,b),q) => (((q+b)/numtilings) :: prevList,b + tilingX2)
      }._1.reverse ++ ints

      hashcoords(coords,intIHT)


    }
  }

  /**
    * :param memory-size: ineteger. the number of possible tile indices
    *
    * @param intIHT integer or IHT object. An index hash table or a positive
    * @param numtilings integer. the number of tilings desired. For best
    *                   results, the second argument, numTilings, should be a power of two
    *                   greater or equal to four times the number of floats
    * @param floats list. a list of real values making up the input vector
    * @param wrapwidths
    * @param ints   list. optional list of integers to get different hashings
    * @return num-tilings tile indices corresponding to the floats and ints,wrapping some floats
    */
  def tileswrap(intIHT: IntIHT, numtilings: Int, floats: Seq[Float], wrapwidths: Seq[Int], ints: List[Int] = Nil): Seq[Int] = {
    val qfloats = floats.map{f => Math.floor(f*numtilings).toInt}
    //println(s"qfloats: ${qfloats}")
    (0 until numtilings).map{ tiling =>
      val tilingX2 = tiling*2


      val coords = qfloats.map(r => Some(r)).zipAll(wrapwidths.map(r => Some(r)),None,None).foldLeft((List(tiling),tiling)) {
        case ((prevList, b), (Some(q), Some(width))) =>

          val c = if(q+b%numtilings >= 0) (q+b%numtilings) / numtilings
          else Math.floor((q+b%numtilings+0d) / numtilings).toInt

          val toAdd = (c%width+width)%width
          //println(s"q: $q width: $width b: $b => c: $c, toAdd: ${toAdd}")
          (toAdd :: prevList, b + tilingX2)
        case ((prevList, b), (Some(q), None)) =>
          val c = if(q+b%numtilings >= 0) (q+b%numtilings) / numtilings
          else Math.floor((q+b%numtilings+0d) / numtilings).toInt
          //println(s"q: $q width: NONE b: $b => c: $c")

          (c :: prevList, b + tilingX2)
        case _ =>
          println(s"ERROR")
          throw  new IllegalArgumentException("|floats| < |wrapwidths|")
      }._1.reverse ++ ints

      //println("Coords: ")
      //println(coords)

      hashcoords(coords,intIHT)


    }
  }

}
