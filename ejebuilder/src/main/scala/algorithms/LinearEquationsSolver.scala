package algorithms

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector, TDirection}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{CircleSegment, TCircleSegment}

object LinearEquationsSolver {

  /**
    * [[a0 b0]  [x]  = [c0]
    *  [a1 b1]] [y]    [c1]
    * @param a0
    * @param b0
    * @param a1
    * @param b1
    * @param c0
    * @param c1
    * @return
    */
  def solve2Variable(a0: Double, b0: Double, a1: Double, b1: Double, c0: Double, c1: Double): Option[(Double,Double)] = {
    val D =  a0*b1 - a1*b0
    val Dx = c0*b1 - c1*b0
    val Dy = a0*c1 - a1*c0
    if(Math.abs(D)>1e-6){
      Some((Dx/D,Dy/D))
    }else{
      None
    }
  }

  def intersection(in: PointUnitaryVector, out: PointUnitaryVector): Option[Point] = {
    solve2Variable(in.direction.dx, out.direction.dx, in.direction.dy, out.direction.dy, out.point.x - in.point.x, out.point.y - in.point.y).flatMap {
      case (distanceIn, distanceOut) => {
        val intersection0 = in.point + in.direction*(distanceIn)
        val intersection1 = out.point + out.direction*(-distanceOut)
        if(intersection0 ==? intersection1){
          Some(intersection0)
        }else{
          None
        }
      }
    }
  }
  def buildCircleSegment(a: Point, b: Point, c: Point): Option[TCircleSegment] = {
    val pu = (a -%- b)
    val pv = (b -%- c)
    val u = (b - a).direction <¬ 1
    val v = (c - b).direction <¬ 1
    val qx = pu.x - pv.x
    val qy = pu.y - pv.y
    val D = (u.dy*v.dx) - (u.dx*v.dy)
    val Du = (-qx*v.dy) + (qy*v.dx)
    val Dv = (u.dx*qy) - (u.dy*qx)

    if(Math.abs(D) > 1e-6){
      val lu = Du/D
      val lv = Dv/D
      val o = pu + ((u <¬ 2)*(lu))
      val oa = a - o
      val ob = b - o
      val oc = c - o

      val anticlockwise = (oa x ob).z > 0


      Some(CircleSegment(a,o,c,anticlockwise))



    }else{
      None
    }
  }
  def buildCircleTangent(in: PointUnitaryVector, out: PointUnitaryVector): Option[TCircleSegment] = {
    val z = in.direction x out.direction
    val res = if(Math.abs(z.z) > 1e-3){

      LinearEquationsSolver.intersection(in,out).flatMap{ intersection => {

        //intersection = in.point + delta_in*in.direction
        val delta_in = intersection - in.point
        val delta_out = intersection - out.point

        val d_in = if(Math.abs(in.direction.dx) > 1e-6) {
          delta_in.dx/in.direction.dx
        }else{
          delta_in.dy/in.direction.dy
        }

        val d_out = if(Math.abs(out.direction.dx) > 1e-6){
          delta_out.dx/out.direction.dx
        }else{
          delta_out.dy/out.direction.dy
        }

        if(d_in.sign == d_out.sign) {
          None
        }else {

          val bisectriz = (in.direction + out.direction).direction <¬ (if ((in.direction x out.direction).z > 0) 1 else 3)

          for {
            centerIn <- LinearEquationsSolver.intersection(PointUnitaryVector(in.point, in.direction <¬ 1), PointUnitaryVector(intersection, bisectriz))
            centerOut <- LinearEquationsSolver.intersection(PointUnitaryVector(out.point, out.direction <¬ 1), PointUnitaryVector(intersection, bisectriz))
          } yield {
            //println(s"centerUN $centerIn")
            //println(s"centerOUT $centerOut")
            if ((!(centerIn - intersection)) < (!(centerOut - intersection))) {
              val u = in.point - centerIn
              val b = intersection - centerIn
              val alpha = b \/ u
              val v = u << (-alpha * 2)
              val outCircle = centerIn + v
              //println(s"inPOint: ${in.point} \nu: $u \nb: $b \nalpha: ${alpha*180/3.14159} \nv: $v \noutCircle: $outCircle")
              //println(s"r0: ${!(in.point  - centerIn)}")
              //println(s"r1: ${!(outCircle - centerIn)}")
              CircleSegment(in.point, centerIn, outCircle, (u.direction x in.direction).z > 0)
            } else {
              val v = out.point - centerOut
              val b = intersection - centerOut
              val alpha = b \/ v
              val u = v << (-alpha * 2)
              val inCircle = centerOut + u
              CircleSegment(inCircle, centerOut, out.point, (u.direction x in.direction).z > 0)
            }
          }
        }

      }
      }

    }else{
      None
    }

    res

  }

  def calcDirections(a: Point, b: Point, c: Point): (TDirection,TDirection,TDirection) = {
    buildCircleSegment(a,b,c) match {
      case Some(circ) => {
        def calcDir(x: Point): TDirection = {
          val v = (x-circ.centerPoint).direction
          if(circ.antiClockWise)
            v <¬ 1
          else
            v <¬ 3
        }
        (calcDir(a),calcDir(b),calcDir(c))
      }
      case None => {
        val d = (c-a).direction
        (d,d,d)
      }

    }
  }
}
