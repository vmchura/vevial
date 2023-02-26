package controller

import forms.SurveyViewerForm
import javafx.scene.{control => jfxsc, layout => jfxsl}
import javafx.{event => jfxe, fxml => jfxf}
import scalafx.Includes._
import scalafx.scene.control.MenuItem
import scalafx.scene.layout.VBox
import scalafx.stage
import scalafx.stage.FileChooser

import java.net.URL
import java.util

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

  private val axisFileChooser = new FileChooser()
  axisFileChooser.extensionFilters += new FileChooser.ExtensionFilter("Land XML", "*.xml")

  @jfxf.FXML
  private var addAxisMenuItemDelegate: jfxsc.MenuItem = _

  @jfxf.FXML
  private var addGPXMenuItemDelegate: jfxsc.MenuItem = _

  @jfxf.FXML
  private var mainVBOXDelegate: jfxsl.VBox = _
  private var mainVBOX: VBox               = _

  @jfxf.FXML
  private def onActionAddAxisMenuItem(event: jfxe.ActionEvent): Unit = {
    val file = axisFileChooser.showOpenDialog(SurveyViewerForm.stage)
    println(file.isFile)
  }

  @jfxf.FXML
  private def onActionAddGPXMenuItem(event: jfxe.ActionEvent): Unit = {
    println("onActionAddGPXMenuItem")
  }

  override def initialize(url: URL, rb: util.ResourceBundle): Unit = {
    mainVBOX = new VBox(mainVBOXDelegate)
  }
}
