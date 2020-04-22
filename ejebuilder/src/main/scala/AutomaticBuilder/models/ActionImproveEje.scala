package AutomaticBuilder.models

sealed trait ActionImproveEje

case class SetPointAt(distanceOverElement: Double,distanceNormal: Double) extends ActionImproveEje with TProjection
object NoAction extends ActionImproveEje
