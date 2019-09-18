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
import scalafx.scene.{Node, Scene}
import scalafx.scene.effect.DropShadow
import scalafx.scene.layout.{BorderPane, HBox, Pane, StackPane}
import scalafx.scene.paint.Color._
import scalafx.scene.paint.{LinearGradient, Stops}
import scalafx.scene.text.Text
import UtilTransformers.PointTransformer._
import scalafx.scene.control.Button
import scalafx.scene.shape.Shape

import scala.collection.mutable.ListBuffer
object HelloScalaFX extends JFXApp {

  val pointAlterTest = List(new ProgresivePoint(Point(150,0),50),
    new ProgresivePoint(Point(200,0),100))

  val r = RectSegment(Point(100,0),Point(200,0))

  val c = CircleSegment(Point(200,0),Point(200,100),Point(200,200),antiClockWise = true)

  val sequence = EfficientSeqEjeElements(TSeqEjeElementsBase().append(r).append(c))


  val sequenceWithRestrictions: EjeEfficientWithRestrictions[TEfficientSeqEjeElements, TEfficientSeqEjeElements] = EjeEfficientWithRestrictions(sequence)

  val pointsProgresive = pointAlterTest.foldLeft(sequenceWithRestrictions){case (sr, pp) => sr.addRestriction(pp) match {
    case Right(value) => value
    case Left(value) => value
  }}




  val eprog: EfficientEjeProgresiva = EfficientEjeProgresiva(pointsProgresive)

  val ejeLayer: EjeVialLayer = new EjeVialLayer(eprog)
  val milestonesLayer = new MilestoneLayer(eprog)

  val layersMerged = new AggregatedObservableArrayList[Node](Array(ejeLayer,milestonesLayer).map(_.nodes))
  val panelMapa = new Pane(){
    children = layersMerged.getAggregatedList()
  }



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
    }



  }

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

