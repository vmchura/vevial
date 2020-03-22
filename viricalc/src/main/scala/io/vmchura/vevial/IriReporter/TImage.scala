package io.vmchura.vevial.IriReporter

import org.joda.time.DateTime

trait TImage {
  def path: String
  def lat: Double
  def lng: Double
  def prog(): Int
  def caption: String
  def date: DateTime
}
