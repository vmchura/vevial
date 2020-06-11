package algorithms

import scala.xml.Elem

trait SectionSerializable[A <: BuilderFixedPoints] {
  def section: A
  def saveToNodeXML(): Elem
}
