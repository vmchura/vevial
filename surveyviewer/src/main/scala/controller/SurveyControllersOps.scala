package controller

import algorithms.MarkSequentialByStepTime
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.TPoint
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.TEfficientSeqEjeElementsProgresiva
import io.vmchura.vevial.elementdata.GPXElementData
import io.vmchura.vevial.relevamiento.SurveyGPX
import javafx.scene.{chart => jfxsch, control => jfxsc, layout => jfxsl}
import javafx.{event => jfxe, fxml => jfxf}
import org.scalafx.extras.{offFX, onFX}
import scalafx.Includes._
import scalafx.scene.chart.{LineChart, NumberAxis, ValueAxis, XYChart}
import scalafx.collections.ObservableBuffer
import algorithms.SurveyProcessorOps.SurveyProcessorOps
import io.vmchura.vevial.EjeVialUtil.Progresiva

import java.time.ZonedDateTime
import scala.concurrent.duration._
object SurveyControllersOps {

  private def calculateProgresive(point: TPoint)(implicit roadAxis: TEfficientSeqEjeElementsProgresiva): Option[Double] = {
    roadAxis.projectPoint(point).map(pp => roadAxis.calcProgresive(pp))
  }
  private def toXYDataAxis(gpxData: GPXElementData)(implicit roadAxis: TEfficientSeqEjeElementsProgresiva): Option[jfxsch.XYChart.Data[Number, Number]] = {
    for {
      originPoint <- gpxData.point
      time <- gpxData.zonedTime
      prog <- calculateProgresive(originPoint.value)
    } yield {
      val z = time.toInstant.toEpochMilli
      XYChart.Data(prog: Number, z: Number).delegate
    }
  }
  def addToAllSurveys(roadAxis: TEfficientSeqEjeElementsProgresiva,
                      survey: SurveyGPX,
                      lineChartDelegate: jfxsch.LineChart[Number, Number],
                      axisYDelegate: jfxsch.NumberAxis,
                      seriesName: String,
                      axisNotAdded: Boolean): Unit = {

    val toXYData : GPXElementData =>  Option[jfxsch.XYChart.Data[Number, Number]] = gpxElementData => toXYDataAxis(gpxElementData)(roadAxis)
    val times = survey.surveyInformation.flatMap(_.zonedTime).map(_.toInstant.toEpochMilli)
    val minY = times.min
    val maxY = times.max
    onFX {
      if (minY < axisYDelegate.getLowerBound | axisNotAdded) {
        axisYDelegate.setLowerBound(minY)
        axisYDelegate.lowerBound = minY
      }
      if (maxY > axisYDelegate.getUpperBound | axisNotAdded) {
        axisYDelegate.setUpperBound(maxY)
        axisYDelegate.upperBound = maxY
      }

      val data = ObservableBuffer[jfxsch.XYChart.Data[Number, Number]](survey.surveyInformation.flatMap(toXYData): _*)
      lineChartDelegate.data.value.add(XYChart.Series(seriesName, data))
    }
  }
  def analyzeSingleSurvey(roadAxis: TEfficientSeqEjeElementsProgresiva,
                          survey: SurveyGPX,
                          lineChartGlobalTimeDelegate: jfxsch.LineChart[Number, Number],
                          lineChartLocalTimeDelegate: jfxsch.LineChart[Number, Number],
                          seriesName: String): () => Unit = {
    () => {
      val listProgressDistanceTimeStamp = survey.fillProgressDistance(roadAxis)
      val progressDistanceTime: List[(Progresiva, Int, ZonedDateTime)] = MarkSequentialByStepTime.
        buildProgresiveByStepTime(listProgressDistanceTimeStamp, survey.duration, 100.millis)

      val toXYData: GPXElementData => Option[jfxsch.XYChart.Data[Number, Number]] = gpxElementData => toXYDataAxis(gpxElementData)(roadAxis)
      def toXYDataLocal(pdt: (Progresiva, Int, ZonedDateTime)): jfxsch.XYChart.Data[Number, Number] = {
        val (progressDistance, timeMillis, _ ) = pdt
        XYChart.Data(progressDistance.progresiva: Number, timeMillis: Number).delegate
      }
      onFX {
        val data = ObservableBuffer[jfxsch.XYChart.Data[Number, Number]](survey.surveyInformation.flatMap(toXYData): _*)
        lineChartGlobalTimeDelegate.data.value.add(XYChart.Series(seriesName, data))
      }
      onFX {
        val data = ObservableBuffer[jfxsch.XYChart.Data[Number, Number]](progressDistanceTime.map(toXYDataLocal): _*)
        lineChartLocalTimeDelegate.data.value.add(XYChart.Series(seriesName, data))
      }

    }
  }
}
