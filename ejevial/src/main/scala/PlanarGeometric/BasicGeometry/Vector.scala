package PlanarGeometric.BasicGeometry

import PlanarGeometric.ConfigParametersGeometric.{areCloseValuesForDirection, areCloseValuesForMagnitude, isMagnitudeValidForUnitaryVector, isValidMagnitudeForGeneralVector}


/**
  * Vector on z axis
  *
  * @param z
  */
case class ZVector(z: Double)


trait TVector[A] {
  def direction: TDirection
  def magnitude: Double
  def dx: Double
  def dy: Double
  def unary_! : Double = magnitude
  //rotates the vector 90 degres to left times
  def <¬ (times: Int): A

  def + (v: TVector[_]): PlanarVector = {
    val (nx,ny,newMagnitude) = getNewValuesAdd(v)
    val newDirection = TDirection(nx/newMagnitude,ny/newMagnitude)
    PlanarVector(newDirection,newMagnitude)
  }

  def +? (v: TVector[_]): Option[PlanarVector] = {
    val (nx,ny,newMagnitude) = getNewValuesAdd(v)
    if(isValidMagnitudeForGeneralVector(newMagnitude)) {
      val newDirection = TDirection(nx / newMagnitude, ny / newMagnitude)
      Some(PlanarVector(newDirection, newMagnitude))
    }else{
      None
    }
  }
  private def getNewValuesAdd(v: TVector[_]): (Double,Double,Double) = {
    val nx = dx + v.dx
    val ny = dy + v.dy
    val newMagnitude = Math.sqrt(nx*nx+ny*ny)
    (nx,ny,newMagnitude)
  }

  def -[B <: TVector[_]](v: TVector[B]): PlanarVector = {
    this + (v <¬ 2)
  }
  def -?[B <: TVector[_]](v: TVector[B]): Option[PlanarVector] = {
    this +? (v <¬ 2)
  }
  //scales the magnitude
  def * (scale: Double): PlanarVector
  def / (scale: Double): PlanarVector

  //dot product
  def * (v: TVector[_]): Double = dx*v.dx+dy*v.dy
  //cross product
  def x (v: TVector[_]): ZVector = ZVector(dx*v.dy - dy*v.dx)
  //positive angle to vector, reuslt in radians
  def \/ (v: TVector[_]): Double = {
    val (vx,vy) =  (v.dx,v.dy)

    val res = Math.atan2(dx * vy - dy * vx, dx * vx + dy * vy)
    if (res < 0)
      2 * Math.PI + res
    else
      res
  }
  //are equivalent?
  def ==? (tv: TVector[_]): Boolean

  //rotate to left by beta angle in radians
  def <<(beta: Double): A

  }

trait TDirection extends TVector[TDirection]




object TDirection {
  def apply(dx: Double, dy: Double): TDirection = {
    if(isMagnitudeValidForUnitaryVector(Math.sqrt(dx*dx+dy*dy)))
      Direction(dx,dy)
    else
      AnyDirection()
  }

  def apply(): TDirection = AnyDirection()

  private case class AnyDirection() extends TDirection {
    override val direction: TDirection = this
    override val magnitude: Double = 0
    override val dx: Double = 0
    override val dy: Double = 0

    override def <¬(times: Int): TDirection = this

    override def *(scale: Double): PlanarVector = PlanarVector(this,0)

    override def /(scale: Double): PlanarVector = PlanarVector(this,0)

    override def ==?(tv: TVector[_]): Boolean = {
      tv match {
        case _ : Direction => true
        case AnyDirection() => true
        case PlanarVector(_,otherMagnitude) => areCloseValuesForMagnitude(otherMagnitude,0)
        case _ => false
      }
    }

    override def <<(beta: Double): TDirection = this
  }
  /**
    * Unitary Vector
    * @param dx
    * @param dy
    */
  private case class Direction(dx: Double, dy: Double) extends TDirection{

    override val direction: Direction = this
    override val magnitude: Double = 1.0


    override def *(scale: Double): PlanarVector = PlanarVector(this,scale)

    override def /(scale: Double): PlanarVector = PlanarVector(this,1.0/scale)

    override def ==?(tv: TVector[_]): Boolean = {
      tv match {
        case Direction(vx,vy) => areCloseValuesForDirection(dx,vx) && areCloseValuesForDirection(dy,vy)
        case AnyDirection() => true
        case _ => false
      }
    }

    override def <<(beta: Double): Direction = {
      val x2 = Math.cos(beta)*dx - Math.sin(beta)*dy
      val y2 = Math.sin(beta)*dx + Math.cos(beta)*dy
      Direction(x2,y2)
    }

    override def <¬(times: Int): Direction = {
      times % 4 match {
        case 0 => this
        case 1 => Direction(-dy,dx)
        case 2 => Direction(-dx,-dy)
        case 3 => Direction(dy,-dx)
      }
    }
  }
}
/**
  *
  * @param direction
  * @param magnitude
  */
case class PlanarVector(direction: TDirection, magnitude: Double) extends TVector[PlanarVector]{
  //require(isValidMagnitude(magnitude))
  override val dx: Double = direction.dx*magnitude
  override val dy: Double = direction.dy*magnitude

  override def <¬(times: Int): PlanarVector = copy(direction = direction <¬ times)

  override def *(scale: Double): PlanarVector = copy(magnitude = magnitude*scale)

  override def /(scale: Double): PlanarVector = copy(magnitude = magnitude/scale)

  override def ==?(tv: TVector[_]): Boolean = tv match {
    case PlanarVector(v,t) => areCloseValuesForMagnitude(magnitude,t) && direction ==? v
    case _ => direction ==? tv
  }

  override def <<(beta: Double): PlanarVector = copy(direction = direction << beta)
}

