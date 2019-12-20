package models

/**
  * Resultado de un segmento IRI, contiene ambos carriles
  * @param progIni
  * @param progFin
  * @param carrilDerecho
  * @param carrilIzquierdo
  */
case class ElementIriResult(progIni: Int, progFin: Int,
                       carrilDerecho: Option[ElementIriResultCarril],
                       carrilIzquierdo: Option[ElementIriResultCarril])
