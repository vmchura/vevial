package io.vmchura.vevial.EjeVialBuilder
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint

class PGVMultipleEjeProjectoSingular extends EfficientEjeByPoints {

  val ejes = List(
    new KMLFolderMultiGeometryToEje("TRAMO_3S-F_UTM"),
    new KMLFolderMultiGeometryToEje("TRAMO_3S-G_UTM")
  )
  override def findProgresiva(point: TPoint): Option[(Option[String], Double, TPoint)] = {
    ejes.flatMap(_.findProgresiva(point)).minByOption(r => r._3.-(point).magnitude)
  }
}
