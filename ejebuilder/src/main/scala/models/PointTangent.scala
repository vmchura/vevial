package models

import algorithms.LinearEquationsSolver
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TDirection
import io.vmchura.vevial.elementdata.{UDirection, UPoint}

sealed trait TPointTangent {
  def point: Option[UPoint]
  def tangent: UDirection
  final def withNext(pOpt: Option[UPoint]): TPointTangent = {
    (this,pOpt) match {
      case (_: PointAlone,Some(next))             =>  PointWithNext(point,next)
      case (PointWithPrev(_,prev),Some(next))   =>  PointWithPrevNext(point,prev,next)
      case (_,Some(_))                            =>  throw new IllegalStateException("Already has next")
      case (_,None)                               =>  this
    }
  }

  final def withPrev(pOpt: Option[UPoint]): TPointTangent = {
    (this,pOpt) match {
      case (_: PointAlone,Some(prev))             =>  PointWithPrev(point,prev)
      case (PointWithNext(_,next),Some(prev))   =>  PointWithPrevNext(point,prev,next)
      case (_,Some(_))                            =>  throw new IllegalStateException("Already has prev")
      case (_,None)                               =>  this
    }
  }

  final def reverse(): TPointTangent = {
    this match {
      case _ : PointAlone => this
      case PointWithNext(p,n) => PointWithPrev(p,n)
      case PointWithPrev(p,n) => PointWithNext(p,n)
      case PointWithPrevNext(p,x,y) => PointWithPrevNext(p,y,x)
    }
  }

  final def withNext(p: UPoint): TPointTangent = withNext(Some(p))
  final def withPrev(p: UPoint): TPointTangent = withPrev(Some(p))
  final def withNext(p: TPointTangent): TPointTangent = withNext(p.point)
  final def withPrev(p: TPointTangent): TPointTangent = withPrev(p.point)
}

object PointTangent {
  val UNKNOW_DIRECTION: UDirection = UDirection(TDirection(),Double.PositiveInfinity)
}


case class PointAlone(point: Option[UPoint]) extends TPointTangent {
  override def tangent: UDirection = PointTangent.UNKNOW_DIRECTION
}

trait TTangentDefinedBy2Points {
  protected def from: Option[UPoint]
  protected def to: Option[UPoint]
  lazy val tangent: UDirection = {
    (for{
      p <- from
      q <- to
      v <- q.value -? p.value
    }yield{
      UDirection(v.direction,Math.atan((p.sigma2+q.sigma2)/ v.magnitude))
    }).getOrElse(PointTangent.UNKNOW_DIRECTION)
  }
}

case class PointWithNext(point: Option[UPoint],next: UPoint) extends TPointTangent with TTangentDefinedBy2Points {
  override protected def from: Option[UPoint] = point

  override protected def to: Option[UPoint] = Some(next)
}
case class PointWithPrev(point: Option[UPoint],prev: UPoint) extends TPointTangent with TTangentDefinedBy2Points {
  override protected def from: Option[UPoint] = Some(prev)

  override protected def to: Option[UPoint] = point
}
case class PointWithPrevNext(point: Option[UPoint], prev: UPoint, next: UPoint) extends TPointTangent {
  override def tangent: UDirection = point.fold(PointTangent.UNKNOW_DIRECTION){ p =>
    val (_,d,_) = LinearEquationsSolver.calcDirections(prev.value,p.value,next.value)
    (next.value -? prev.value).fold(UDirection(TDirection(),Double.PositiveInfinity)){ v =>
      UDirection(d,Math.atan((p.sigma2+prev.sigma2+next.sigma2)/ v.magnitude))
    }

  }
}



