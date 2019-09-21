import EjeVialBuilder.LandXMLToEje
import Layers.{AggregatedObservableArrayList, EjeVialLayer, MilestoneLayer}
import UtilTransformers.PointTransformer
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.{Node, Scene}
import scalafx.scene.layout.{BorderPane, Pane}
import UtilTransformers.PointTransformer._
import scalafx.scene.control.Button
import scalafx.Includes._

import scala.io.Codec
import scala.jdk.CollectionConverters._
import scala.reflect.io.File
object HelloScalaFX extends JFXApp {

  val file = File("/home/vmchura/Documents/001.Projects/Vevial/ejevialview/src/main/resources/testTramo.xml")

  val arraySeqNodes: Array[ObservableBuffer[Node]] = {
    new LandXMLToEje(file.reader(Codec("UTF-8"))).toEje match {
      case Right(value) =>
        offsetX() = value.elements.head.in.point.x
        offsetY() = value.elements.head.in.point.y
        val ejeLayer: EjeVialLayer = new EjeVialLayer(value)
        val milestonesLayer = new MilestoneLayer(value)
        Array(ejeLayer,milestonesLayer).map(_.nodes)
      case _ => Array()
    }
  }


  val panelMapa = new Pane()
  val layersMerged = new AggregatedObservableArrayList[Node](arraySeqNodes)

  
  val lastPositionX = new ObjectProperty[Option[Double]](this,"lastPositionX",None)
  val lastPositionY = new ObjectProperty[Option[Double]](this,"lastPositionY",None)
  panelMapa.onMouseDragged = ae => {



    for{
      lx <- lastPositionX()
      ly <- lastPositionY()
    }yield{
      offsetX() = offsetX() - (ae.getX-lx) *factor()
      offsetY() = offsetY() + (ae.getY-ly) *factor()
    }
    lastPositionX() = Some(ae.getX)
    lastPositionY() = Some(ae.getY)


  }
  panelMapa.onMouseReleased = _ => {

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

