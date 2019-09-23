import EjeVialBuilder.LandXMLToEje
import Layers.{EjeVialLayer, MilestoneLayer, ObservableListDelegate, SimpleRelevamientoLayer}
import UtilTransformers.PointTransformer
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.{Node, Scene}
import scalafx.scene.layout.{BorderPane, Pane}
import UtilTransformers.PointTransformer._
import relevamiento.RelevamientoIRI
import scalafx.scene.control.Button

import scala.io.Codec
import scala.reflect.io.File
object HelloScalaFX extends JFXApp {

  val fileXML = File("/home/vmchura/Documents/001.Projects/Vevial/ejevialview/src/test/resources/tramo123.xml")
  val fileCSV = new java.io.File("/home/vmchura/Documents/001.Projects/Vevial/relevamientodata/src/test/resources/2019-03-05 14h36m22s Survey.csv")

  val arraySeqNodes: Array[ObservableBuffer[Node]] = {
    new LandXMLToEje(fileXML.reader(Codec("UTF-8"))).toEje match {
      case Right(value) =>
        offsetX() = value.elements.head.in.point.x
        offsetY() = value.elements.head.in.point.y
        val relevamientoIRI = new SimpleRelevamientoLayer(new RelevamientoIRI(fileCSV))
        val ejeLayer: EjeVialLayer = new EjeVialLayer(value)
        val milestonesLayer = new MilestoneLayer(value)
        Array(ejeLayer,milestonesLayer,relevamientoIRI).map(_.nodes)
      case _ => Array()
    }
  }


  val panelMapa = new Pane()
  endX.unbind()
  iniY.unbind()

  endX <== convertXView2Real(panelMapa.width)
  iniY <== convertYView2Real(panelMapa.height)

  val layersMerged = new ObservableListDelegate(arraySeqNodes,panelMapa.children)

  
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

