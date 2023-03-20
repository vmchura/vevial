package controller

import Layers.{EjeVialLayer, MilestoneLayer, ProjectionSurveyLayer}
import ScalaFXControllers.PanelLayered
import forms.SurveyViewerForm
import io.vmchura.vevial.EjeVialBuilder.LandXMLToEje
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.{EfficientEjeProgresiva, TEfficientSeqEjeElementsProgresiva}
import io.vmchura.vevial.elementdata.GPXElementData
import io.vmchura.vevial.relevamiento.SurveyGPX
import javafx.scene.{chart => jfxsch, control => jfxsc, layout => jfxsl}
import javafx.{event => jfxe, fxml => jfxf}
import org.scalafx.extras.{offFX, onFX}
import scalafx.Includes._
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.{LineChart, NumberAxis, ValueAxis, XYChart}
import scalafx.scene.control.{Label, ListView}
import scalafx.scene.layout.VBox
import scalafx.stage.FileChooser
import scalafx.util.StringConverter

import java.net.URL
import java.util
import scala.io.Codec
import scala.reflect.io.File

/**
 * Example of a controlled initialized through FXML.
 *
 * When working with FXML, due to the nature of JavaFX FXMLLoader, we need to expose variables and methods that
 * FXMLLoader will be using with JavaFX signatures.
 *
 * The FXMLLoader injects JavaFX objects as values of member variables marked with annotation `@jfxf.FXML`.
 * We need to declare those variables using JavaFX types (not ScalaFX types).
 * We can use those variables directly or wrap them in ScalaFX objects.
 * Here, for the sake of illustration, we only wrap one variable `gridDelegate` (it is not strictly necessary).
 * The most convenient place to do wrapping is in the overloaded method `initialize`. It is executed after
 * FXMLLoader injects its objects.
 *
 * We can rely on ScalaFX "magic" to use ScalaFX methods on variables that were not explicitly wrapped.
 * All we need to do is to "summon the magic" using "import scalafx.Includes._".
 * This is demonstrated in method "handleClear" where we access properties on
 * JavaFX objects using ScalaFX way, no `get` or `set` involved.
 *
 * Methods annotated with `@jfxf.FXML`, that will be wired to event handlers by FLXMLoader.
 * They need to use JavaFX method signatures. This is illustrated in methods: `handleSubmit` and  `handleClear`.
 *
 * In the rest of the code we can use ScalaFX, for instance, to create more in the event handlers or bind
 * properties.
 *
 * @author Jarek Sacha
 */
class SurveyAnalyserController extends jfxf.Initializable {
  private val axisFileChooser = new FileChooser()
  axisFileChooser.extensionFilters += new FileChooser.ExtensionFilter("Land XML", "*.xml")
  private val gpxFileChooser = new FileChooser()
  gpxFileChooser.extensionFilters += new FileChooser.ExtensionFilter("GPX", "*.gpx")
  @jfxf.FXML
  private var lineChartDelegate: jfxsch.LineChart[Number, Number] = _
  @jfxf.FXML
  private var axisXDelegate: jfxsch.NumberAxis = _
  @jfxf.FXML
  private var axisYDelegate: jfxsch.NumberAxis = _
  private var axisNotAdded = true

  @jfxf.FXML
  private var listViewSourcesDelegate: jfxsc.ListView[String] = _
  private var listViewSources: ListView[String] = _
  private val listSources = ObservableBuffer[String]()
  private val listSourcesProperty = new ObjectProperty[ObservableBuffer[String]](null, "", listSources)

  var lastRoadAxisBuilt: Option[TEfficientSeqEjeElementsProgresiva] = None

  @jfxf.FXML
  private def onActionAddAxisMenuItem(event: jfxe.ActionEvent): Unit = {
    val javaFile = axisFileChooser.showOpenDialog(SurveyViewerForm.stage)
    if(javaFile.isFile) {
      listSources += s"RoadAxis ${listSources.length}"
      offFX {
        val file = File(javaFile)
        val ejeEither: Either[Exception, EfficientEjeProgresiva] = new LandXMLToEje(file.reader(Codec("UTF-8"))).toEje
        println("Eje loaded finished")
        onFX{
          ejeEither.foreach { eje =>
            lastRoadAxisBuilt = Some(eje)
            println(s"Setting ${eje.minProg} -> ${eje.maxProg}")
            axisXDelegate.setLowerBound(eje.minProg)
            axisXDelegate.setUpperBound(eje.maxProg)
            axisXDelegate.lowerBound =  eje.minProg
            axisXDelegate.upperBound =  eje.maxProg
            println("Eje appended")

          }
        }

      }

    }else{
      println("No file was selected")
    }
  }
  @jfxf.FXML
  private def onActionAddGPXMenuItem(event: jfxe.ActionEvent): Unit = {
    val listFiles = gpxFileChooser.showOpenMultipleDialog(SurveyViewerForm.stage)
    if (lastRoadAxisBuilt.nonEmpty) {
      listFiles.filter(_.isFile).map { javaFile =>
        listSources += javaFile.getName
        lastRoadAxisBuilt.foreach { roadAxis =>
          println("Loading survey")
          SurveyGPX(javaFile.getPath) match {
            case Left(errors) => errors.foreach(println)
            case Right(survey) =>
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
              if(minY < axisYDelegate.getLowerBound | axisNotAdded){
                axisYDelegate.setLowerBound(minY)
                axisYDelegate.lowerBound = minY
              }
              if(maxY > axisYDelegate.getUpperBound | axisNotAdded){
                axisYDelegate.setUpperBound(maxY)
                axisYDelegate.upperBound = maxY
              }
              axisNotAdded = false
              val data = ObservableBuffer[jfxsch.XYChart.Data[Number, Number]](survey.surveyInformation.flatMap(toXYData): _*)
              lineChartDelegate.data.value.add(XYChart.Series(javaFile.getName, data))
          }

        }
      }
    }

  }


  override def initialize(url: URL, rb: util.ResourceBundle): Unit = {
    listViewSources = new ListView(listViewSourcesDelegate)
    listViewSources.items <==> listSourcesProperty

//    lineChart = new LineChart(lineChartDelegate)
//    lineChartDelegate.getXAxis.asInstanceOf[jfxsch.ValueAxis[Number]].
//      setTickLabelFormatter(new StringConverter[Number] {
//      override def fromString(string: String): Number = string.toLong
//
//      override def toString(t: Number): String = t.longValue().toString
//    })
  }
}
