package forms

import javafx.{fxml => jfxf, scene => jfxs}
import scalafx.Includes._
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.scene.Scene

import java.io.IOException

/**
 * Example of using FXMLLoader from ScalaFX.
 *
 * @author Jarek Sacha
 */
object SurveyViewerForm extends JFXApp3 {

  def start(): Unit = {

    val resource = getClass.getResource("/SurveyViewer.fxml")

    if (resource == null) {
      throw new IOException("Cannot load resource: SurveyViewer.fxml")
    }

    // NOTE: ScalaFX doe not yet provide a wrapper fro FXMLLoader (2012.11.12)
    // We load here FXML content using JavaFX directly.
    // It is important to provide type for the element loaded,
    // though it can be a generic, here use `javafx.scene.parent`.

    val root: jfxs.Parent =  jfxf.FXMLLoader.load(resource)

    stage = new PrimaryStage() {
      title = "FXML VBox Demo"
      scene = new Scene(root)
    }
  }

}