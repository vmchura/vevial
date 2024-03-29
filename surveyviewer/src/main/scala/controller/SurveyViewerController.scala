package controller

import Layers.{DummyLayer, EjeVialLayer, MilestoneLayer, ProjectionSurveyLayer}
import ScalaFXControllers.PanelLayered
import forms.SurveyViewerForm
import io.vmchura.vevial.EjeVialBuilder.{LandXMLToEje, LandXMLWithRestrictionsToEje}
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.{EfficientEjeProgresiva, TEfficientSeqEjeElementsProgresiva}
import io.vmchura.vevial.relevamiento.SurveyGPX
import javafx.collections.ObservableList
import javafx.scene.{control => jfxsc, layout => jfxsl}
import javafx.{event => jfxe, fxml => jfxf}
import org.scalafx.extras.{offFX, onFX, onFXAndWait}
import scalafx.Includes._
import scalafx.beans.property.{ObjectProperty, ReadOnlyObjectProperty}
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.{Label, ListCell, ListView}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.stage.FileChooser

import java.io.FileInputStream
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
class SurveyViewerController extends jfxf.Initializable {
  @jfxf.FXML
  private var mapPaneDelegate: jfxsl.Pane = _
  private var mapPane: PanelLayered = _
  private val axisFileChooser = new FileChooser()
  axisFileChooser.extensionFilters += new FileChooser.ExtensionFilter("Land XML", "*.xml")
  private val gpxFileChooser = new FileChooser()
  gpxFileChooser.extensionFilters += new FileChooser.ExtensionFilter("GPX", "*.gpx")
  @jfxf.FXML
  private var mainVBOXDelegate: jfxsl.VBox = _
  private var mainVBOX: VBox               = _

  @jfxf.FXML
  private var listViewSourcesDelegate: jfxsc.ListView[String] = _
  private var listViewSources: ListView[String] = _
  private val listSources = ObservableBuffer[String]()
  private val listSourcesProperty = new ObjectProperty[ObservableBuffer[String]](null, "", listSources)

  var lastRoadAxisBuilt: Option[TEfficientSeqEjeElementsProgresiva] = None

  @jfxf.FXML
  private var viewLabelDelegate: jfxsc.Label = _
  private var viewLabel: Label = _
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
            val hitoLayer = new MilestoneLayer(eje)
            val ejeLayer = new EjeVialLayer(eje)
            mapPane.appendLayer(hitoLayer)
            mapPane.appendLayer(ejeLayer)

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
    val javaFile = gpxFileChooser.showOpenDialog(SurveyViewerForm.stage)
    if (javaFile.isFile && lastRoadAxisBuilt.nonEmpty) {
      listSources += s"GPX ${listSources.length}"
      lastRoadAxisBuilt.foreach{ roadAxis =>
        println("Loading survey")
        SurveyGPX(javaFile.getPath) match {
          case Left(errors) => errors.foreach(println)
          case Right(survey) =>
            println("Adding survey layer")
            val projectionLayer = new ProjectionSurveyLayer(survey, roadAxis)
            mapPane.appendLayer(projectionLayer)
            println("Eje appended")
        }

      }
    }

  }

  @jfxf.FXML
  private def onActionScaleAll(event: jfxe.ActionEvent): Unit = {

    mapPane.scaleAll()
  }

  override def initialize(url: URL, rb: util.ResourceBundle): Unit = {
    mainVBOX = new VBox(mainVBOXDelegate)
    listViewSources = new ListView(listViewSourcesDelegate)
    listViewSources.items <==> listSourcesProperty
    mapPane = new PanelLayered(mapPaneDelegate)
    viewLabel = new Label(viewLabelDelegate)
    viewLabel.text <==> mapPane.pointTransformer.viewTextProperty
  }
}
