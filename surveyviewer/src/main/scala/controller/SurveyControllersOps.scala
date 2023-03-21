package controller

import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.TEfficientSeqEjeElementsProgresiva
import io.vmchura.vevial.elementdata.GPXElementData
import io.vmchura.vevial.relevamiento.SurveyGPX
import javafx.scene.{chart => jfxsch, control => jfxsc, layout => jfxsl}
import javafx.{event => jfxe, fxml => jfxf}
import org.scalafx.extras.{offFX, onFX}
import scalafx.Includes._
import scalafx.scene.chart.{LineChart, NumberAxis, ValueAxis, XYChart}
import scalafx.collections.ObservableBuffer

object SurveyControllersOps {
  def addToAllSurveys(roadAxis: TEfficientSeqEjeElementsProgresiva,
                      survey: SurveyGPX,
                      lineChartDelegate: jfxsch.LineChart[Number, Number],
                      axisYDelegate: jfxsch.NumberAxis,
                      seriesName: String,
                      axisNotAdded: Boolean): Unit = {
    def toXYData(gpxData: GPXElementData): Option[jfxsch.XYChart.Data[Number, Number]] = {
      for {
        originPoint <- gpxData.point
        time <- gpxData.zonedTime
        prog <- roadAxis.projectPoint(originPoint.value).map(pp => roadAxis.calcProgresive(pp))
      } yield {
        val z = time.toInstant.toEpochMilli
        XYChart.Data(prog: Number, z: Number).delegate
      }
    }

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
  def analyzeSingleSurvey(): () => Unit = {
    () => {
      println("call me!!!")
    }
  }
}
