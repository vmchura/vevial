package gym.evialgame

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{PlanarVector, Point, TDirection}
import io.vmchura.vevial.elementdata.UPlanarVector

/**
  *
  * @param originalPoint: Point2D where is the point
  */
case class RopeNode(originalPoint: Point){
/*
  /**
    *
    * @param beta         :to add (alpha + beta)
    * @param oldNextNode  the old value
    * @param w            : 0 => oldNextNode
    *                      oo => exactNextValue
    * @return
    */
  def nextPoint(beta: Double,oldNextNode: RopeNode,w: Double): Point = {
    val theta = beta+alpha
    val toAddX = RopeEje.distanceBetweenNodes*Math.cos(theta)
    val toAddY = RopeEje.distanceBetweenNodes*Math.sin(theta)
    val p = point + PlanarVector(TDirection(toAddX,toAddY),1d)
    RopeNode.promediar(oldNextNode.point,p,w)
  }
  */
  /**
    *
    * @param newPoint, where the new position is
    * @param torque, the torque to apply, torque -> oo, alpha -> pi/4, torque -> -oo, alpha -> -pi/4, torque -> 0, alpha -> 0
    * @param force: force -> oo => desp -> exact
    *               force -> 0 => desp -> 0
    *
    *               2 -> 1
    *               1 -> 0.75
    *               0.5 -> 0.5
    *
    * @return
    */
  def turnNode(newPoint: Point, torque: Double, force: Double, nextPoint: Point): Point = {
    val dx = (newPoint.x-originalPoint.x)*Math.tanh(force)
    val dy = (newPoint.y-originalPoint.y)*Math.tanh(force)
    val nextPointTranslated = Point(nextPoint.x+dx,nextPoint.y+dy)
    val alpha = Math.tanh(torque)*Math.PI/16

    val originalDistance = (nextPoint - originalPoint).magnitude
    newPoint + ((nextPointTranslated-newPoint) << alpha).direction*originalDistance



  }




  override def toString: String = f"[${originalPoint.x}%4.1f, ${originalPoint.y}%4.1f]"
}
object RopeNode{
  /**
    * a --(w)--- x -----(1)---- b
    * @param w    : 0 => a
    *               oo => b
    * @return
    */
  private def promediar(a: Point, b: Point, w: Double): Point = {
    val distanceOpt = b-?a
    distanceOpt.map{ vector =>
      val d = vector.magnitude*w/(w+1d)
      a+vector.direction*d

    }.getOrElse(a)
  }
}
