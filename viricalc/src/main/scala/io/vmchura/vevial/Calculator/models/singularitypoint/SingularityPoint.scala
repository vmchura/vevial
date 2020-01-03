package io.vmchura.vevial.Calculator.models.singularitypoint

import java.util.UUID

case class SingularityPoint(groupID: UUID,id: Int,
                            description: String,
                            progIni: Int, progFin: Int, delta: Int, order: Int){
  def containsPoint(progresiva: Int): Boolean = progIni - delta <= progresiva && progresiva <= progFin + delta
}


