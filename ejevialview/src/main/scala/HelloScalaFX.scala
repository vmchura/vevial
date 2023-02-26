import io.vmchura.vevial.EjeVialBuilder.LandXMLToEje
import Layers.{EjeVialLayer, MilestoneLayer, ObservableListDelegate, ProjectionIRIRelevamientoLayer}
import UtilTransformers.PointTransformer
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.{Node, Scene}
import scalafx.scene.layout.{BorderPane, Pane}
import UtilTransformers.PointTransformer
import io.vmchura.vevial.elementdata.IRIElementData
import io.vmchura.vevial.relevamiento.RelevamientoIRI
import scalafx.scene.paint.Color
import scalafx.scene.control.Button

import scala.io.Codec
import scala.reflect.io.File
object HelloScalaFX extends JFXApp {
  val pointTransformer = new PointTransformer(null, null)

  import pointTransformer._
  private val fileXML = File("/home/vmchura/Documents/001.Projects/vevial/ejevialview/src/test/resources/tramo123.xml")
  private val fileCSV1 = new java.io.File("/home/vmchura/Documents/My Roughometer/Survey Results/2018-08-01/2018-08-01 10h02m32s Survey.csv")
  private val fileCSV2 = new java.io.File("/home/vmchura/Documents/My Roughometer/Survey Results/2018-08-01/2018-08-01 13h24m33s Survey.csv")

  val arraySeqNodes: Array[ObservableBuffer[Node]] = {
    new LandXMLToEje(fileXML.reader(Codec("UTF-8"))).toEje match {
      case Right(value) =>
        val eje = value//.slice(95000,96950)

        offsetX() = eje.elements.head.in.point.x
        offsetY() = eje.elements.head.in.point.y
        val relIRI1 = RelevamientoIRI(fileCSV1,cd => IRIElementData(cd)).sliceBy(eje.leftmostPoint.x,eje.rightmostPoint.x,eje.lowerPoint.y,eje.upperPoint.y)
        val relIRI2 = RelevamientoIRI(fileCSV2,cd => IRIElementData(cd)).sliceBy(eje.leftmostPoint.x,eje.rightmostPoint.x,eje.lowerPoint.y,eje.upperPoint.y)
        val relevamientoIRI1 = new ProjectionIRIRelevamientoLayer(relIRI1,eje, Color.Blue)
        val relevamientoIRI2 = new ProjectionIRIRelevamientoLayer(relIRI2,eje, Color.Magenta)
        val ejeLayer: EjeVialLayer = new EjeVialLayer()
        ejeLayer.setEje(eje)
        val milestonesLayer = new MilestoneLayer(eje)
        Array(
          ejeLayer,
          milestonesLayer,
          relevamientoIRI1,
          relevamientoIRI2
        ).map(_.nodes)
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
      val px = pointTransformer.convertXView2Real(ae.getX)
      val py = pointTransformer.convertYView2Real(ae.getY)
      pointTransformer.updateOffsetWithPivot(newFactor,px,py)
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

