package io.vmchura.vevial.elementdata

trait TElementWithPoint[A <: TElementWithPoint[A]] extends TElementData[A] with DataWithPoint { self: A =>

}
