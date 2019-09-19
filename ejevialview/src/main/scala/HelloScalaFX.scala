import EjeVialBuilder.SimpleConvertibleToEje
import Layers.{AggregatedObservableArrayList, EjeVialLayer, MilestoneLayer, TLayer}
import PlanarGeometric.BasicEje.{EfficientSeqEjeElements, TEfficientSeqEjeElements, TSeqEjeElementsBase}
import PlanarGeometric.BasicGeometry.Point
import PlanarGeometric.EjeElement.{CircleSegment, RectSegment}
import PlanarGeometric.ProgresiveEje.{EfficientEjeProgresiva, WithProgresive}
import PlanarGeometric.RestrictiveEje.{EjeEfficientWithRestrictions, ProgresivePoint}
import ShapeGenerator.{EjeConverter, SimpleEjeElementConverter}
import UtilTransformers.PointTransformer
import javafx.collections.ObservableList
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.{Group, Node, Scene}
import scalafx.scene.effect.DropShadow
import scalafx.scene.layout.{BorderPane, HBox, Pane, StackPane}
import scalafx.scene.paint.Color._
import scalafx.scene.paint.{LinearGradient, Stops}
import scalafx.scene.text.Text
import UtilTransformers.PointTransformer._
import scalafx.scene.control.Button
import scalafx.scene.shape.Shape
import scalafx.Includes._

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer
object HelloScalaFX extends JFXApp {

  val eprog = new SimpleConvertibleToEje().toEje()
  val ejeLayer: EjeVialLayer = new EjeVialLayer(eprog)
  val milestonesLayer = new MilestoneLayer(eprog)
  val panelMapa = new Pane()
  val layersMerged = new AggregatedObservableArrayList[Node](Array(ejeLayer,milestonesLayer).map(_.nodes))

  
  val lastPositionX = new ObjectProperty[Option[Double]](this,"lastPositionX",None)
  val lastPositionY = new ObjectProperty[Option[Double]](this,"lastPositionY",None)
  panelMapa.onMouseDragged = ae => {



    for{
      lx <- lastPositionX()
      ly <- lastPositionY()
    }yield{
      offsetX() = offsetX() - ((ae.getX-lx))*factor()
      offsetY() = offsetY() + ((ae.getY-ly))*factor()
    }
    lastPositionX() = Some(ae.getX)
    lastPositionY() = Some(ae.getY)


  }
  panelMapa.onMouseReleased = ae => {

    lastPositionX() = None
    lastPositionY() = None
  }

  def updateItemsDrawn(): Unit = {
    val (added,removed) = layersMerged.getAddedAndDeleted()
    panelMapa.children.removeAll(removed.map(_.delegate).asJavaCollection)
    panelMapa.children.appendAll(added.map(_.delegate))
  }

  panelMapa.onScroll = ae => {



    val newFactorOpt: Option[Double] = ae.getDeltaY match {
      case positive: Double if positive > 0.1 => Some(factor()*Math.log(2.0))
      case negative: Double if negative < -0.1 => Some(factor()/Math.log(2.0))
      case _ => None
    }

    newFactorOpt.foreach{ newFactor =>
      val px = PointTransformer.convertXView2Real(ae.getX)
      val py = PointTransformer.convertYView2Real(ae.getY)
      PointTransformer.updateOffsetWithPivot(newFactor,px,py)
      //panelMapa.children = layersMerged.getAggregatedList()
      updateItemsDrawn()
    }



  }
  updateItemsDrawn()
  stage = new PrimaryStage {
    title = "ScalaFX Hello World"
    width = 600
    height = 450
    scene = new Scene {

      content = new BorderPane() {
        left = new Button("left")
        top = new Button("top")
        center =  panelMapa
      }
    }
  }
}

