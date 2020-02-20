package io.vmchura.vevial.elementdata

import io.vmchura.vevial.EjeVialUtil.Coordinates
import io.vmchura.vevial.EjeVialUtil.UtilFunctions.str2Double
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point
import io.vmchura.vevial.relevamiento.RelevamientoConfig.sigma2DefaultGPSRoughmeterIII

trait TCrudeIRIData{
  def sectionID: String
  def subdistanceStr: String
  def totalDistanceStr: String
  def iriSTR: String
  def speedSTR: String
  def latitudeSTR: String
  def longitudeSTR: String
  def altituteSTR: String
  def eventsSTR: String
}

class CrudeIRIData(lineData: String) extends TCrudeIRIData{
  private val cols = lineData.split(",").map(_.trim)
  private val colsFilled = cols ++ Array.fill(9-cols.length)("")

  private val Array(secID, subDist, totalDist, iri, speed, latitudStr, longitudStr, altitudStr, eventsStr) = colsFilled

  override def sectionID: String = secID

  override def subdistanceStr: String = subDist

  override def totalDistanceStr: String = totalDist

  override def iriSTR: String = iri

  override def speedSTR: String = speed

  override def latitudeSTR: String = latitudStr

  override def longitudeSTR: String = longitudStr

  override def altituteSTR: String = altitudStr

  override def eventsSTR: String = eventsStr

}

