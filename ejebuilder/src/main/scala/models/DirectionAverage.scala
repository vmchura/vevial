package models

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TDirection.{AnyDirection, Direction}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{PlanarVector, TDirection}

import scala.collection.mutable.ListBuffer


class DirectionAverage(){
  val allValues = ListBuffer.empty[PlanarVector]
  def add(t: TDirection): Unit = allValues += PlanarVector(t.direction,1)


  def value(): TDirection  = {
    if(allValues.nonEmpty) {
      if(allValues.exists { _.direction match
        {
          case AnyDirection() => true
          case _ => false
        }
      }){
        TDirection()
      }else{
        allValues.reduceLeft(_ + _).direction
      }

    } else
      TDirection()
  }

}