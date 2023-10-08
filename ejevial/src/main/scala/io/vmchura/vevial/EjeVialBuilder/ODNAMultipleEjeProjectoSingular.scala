package io.vmchura.vevial.EjeVialBuilder

import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint

class ODNAMultipleEjeProjectoSingular extends EfficientEjeByPoints {
  val ejes = List(
    new KMLMultipleContinuosToEje("KM 3SF", List("Tramo I", "Tramo II", "Tramo IIIa")),
    new KMLMultipleContinuosToEje("KM 3SG", List("Tramo IIIb", "Tramo IVa", "Tramo IVb", "Tramo V", "Tramo VI", "Tramo VII", "Tramo VIII", "")),
  )

  override def findProgresiva(point: TPoint): Option[(Option[String], Double, TPoint)] = {
    ejes.flatMap(_.findProgresiva(point)).minByOption(r => r._3.-(point).magnitude)
  }
}
