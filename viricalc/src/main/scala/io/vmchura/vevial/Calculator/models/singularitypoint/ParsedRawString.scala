package io.vmchura.vevial.Calculator.models.singularitypoint

case class ParsedRawString(originalValues: Array[String]) {

  def this(alreadyParsedLine: String) = this(alreadyParsedLine.split(ParsedRawString.safeSeparator))
  val stringToSave = originalValues.mkString(ParsedRawString.safeSeparator)
}
object ParsedRawString{
  private val safeSeparator = "///"

}
