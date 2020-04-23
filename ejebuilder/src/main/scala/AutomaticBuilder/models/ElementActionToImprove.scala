package AutomaticBuilder.models

case class ElementActionToImprove(elementCanImprove: TElementCanImprove,actionImproveEje: ActionImproveEje) extends Ordered[ElementActionToImprove] {
  override def compare(that: ElementActionToImprove): Int = {
    (actionImproveEje,that.actionImproveEje) match {
      case (NoAction, NoAction) => 0
      case (NoAction, SetPointAt(_, _)) => -1
      case (SetPointAt(_, _), NoAction) => 1
      case (SetPointAt(_,d0), SetPointAt(_,d1)) => d0.abs.compareTo(d1.abs)
    }
  }
}

