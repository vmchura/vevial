package io.vmchura.vevial.IriReporter

trait IriValueAfterProcess {
  def progresiva: Int
  def iriValue: Option[Double]
  def description: Option[String]
  def itemOnFile: Int
  def fileTag: String
}

object IriValueAfterProcess{
  case class ValueComplete(itemOnFile: Int, fileTag: String,progresiva: Int, iriValue: Some[Double], description: Option[String]) extends IriValueAfterProcess
  case class ValueDeleted(itemOnFile: Int, fileTag: String,progresiva: Int, description: Some[String]) extends IriValueAfterProcess {
    override def iriValue: Option[Double] = None
  }

  def apply(itemOnFile: Int, fileTag: String, progresiva: Int, iriValue: Some[Double], description: Option[String]): IriValueAfterProcess = {
    ValueComplete(itemOnFile: Int, fileTag: String, progresiva: Int, iriValue: Some[Double], description: Option[String])
  }
  def apply(itemOnFile: Int, fileTag: String, progresiva: Int, description: Some[String]): IriValueAfterProcess = {
    ValueDeleted(itemOnFile: Int, fileTag: String, progresiva: Int, description: Some[String])
  }
}
