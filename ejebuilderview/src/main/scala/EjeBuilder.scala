import scalafx.Includes._
import Layers.{EjeVialLayer, GeoNodeLayer, InitialDraftLayer, LinkLayer, ObservableListDelegate, SimpleIRIRelevamientoLayer}
import UtilTransformers.PointTransformer
import io.vmchura.vevial.elementdata.IRIElementData
import io.vmchura.vevial.relevamiento.RelevamientoIRI
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.collections.ObservableBuffer
import scalafx.scene.{Node, Scene}
import scalafx.scene.control.{Alert, Button}
import scalafx.scene.layout.{Background, BackgroundFill, BorderPane, CornerRadii, Pane}
import UtilTransformers.PointTransformer._
import algorithms.{DiscreteRelevamiento, EjeBuilderDraft}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.TEfficientSeqEjeElementsProgresiva
import javafx.scene.input
import models.{GeoNode, LinearGraph, LinearGraphEditable, MutableEje}
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.Insets
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.input.TransferMode
import scalafx.scene.paint.Color

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer

object EjeBuilder extends JFXApp{

  val relevamientosAdded = ListBuffer.empty[RelevamientoIRI[IRIElementData]]
  val linkLayer = new LinkLayer()
  val geoNodeLayer = new GeoNodeLayer()

  offsetX() = 0d
  offsetY() = 0d

  var linearGraphEditable = Option.empty[LinearGraphEditable]

  def buildEje(): MutableEje = {
    val ejeBuilder = new EjeBuilderDraft[RelevamientoIRI[IRIElementData],IRIElementData](relevamientosAdded.toList)
    ejeBuilder.buildEje()
  }




  def loadNewFile(relevamientos: Seq[RelevamientoIRI[IRIElementData]]): Unit = {
    relevamientos.foreach{ relevamiento =>
      relevamientosAdded.append(relevamiento)
    }
    val relevamientosSimples = relevamientos.map(x => new SimpleIRIRelevamientoLayer(x))

    linearGraphEditable.foreach{_.clear()}
    linearGraphEditable = None

    try{
      val nodeEje: Seq[LinearGraph[GeoNode]] = DiscreteRelevamiento.convertIntoDiscreteRelevamiento[RelevamientoIRI[IRIElementData],IRIElementData,GeoNode](relevamientosAdded.toList)
      val singleLinearEje = LinearGraph.mergeLinearGraphs(nodeEje)

      linearGraphEditable = Some(LinearGraphEditable(singleLinearEje.nodes,linkLayer,geoNodeLayer))

      offsetX() = singleLinearEje.nodes.head.center.x
      offsetY() = singleLinearEje.nodes.head.center.y
    }catch{
      case error: Throwable =>  {
        println(error.toString)
        new Alert(AlertType.Information, "No se pudo crear el primer bosquejo").showAndWait()
      }
    }

    new ObservableListDelegate(relevamientosSimples.toArray.map(_.nodes),panelMapa.children)

  }

  //val relevamientoFile = new java.io.File("/home/vmchura/Documents/003.CVSC/IRI/Auomated/2020-02-21 12h50m20s Survey T1 HIZQ.csv")
  //val relevamientoFile2 = new java.io.File("/home/vmchura/Documents/003.CVSC/IRI/Auomated/2020-02-21 12h13m18s Survey T1 HDER.csv")


    //val relevamientoIRI = RelevamientoIRI(relevamientoFile,cd => IRIElementData(cd))

  val panelMapa = new Pane(){

    background = new Background(Array(new BackgroundFill(Color.LightGrey,CornerRadii.Empty, Insets.Empty)))

    onDragOver = e => {

      if( e.getDragboard.hasFiles){
        val de = new scalafx.scene.input.DragEvent(e)
        val t: Array[input.TransferMode] = TransferMode.CopyOrMove
        de.acceptTransferModes(t: _*)


      }
    }

    onDragDropped = e => {

      val db = e.getDragboard
      if(db.hasFiles){
        val relevamientos = db.getFiles.asScala.flatMap{ file =>
          try{
            Some(RelevamientoIRI(file,cd => IRIElementData(cd)))
          }catch{
            case _: Throwable => None
          }
        }
        println("Adding files")
        loadNewFile(relevamientos.toList)
        e.setDropCompleted(true)
        e.consume()

      }else{
        println("No files")
      }
    }
    minWidth = 600
    minHeight = 450
  }


  new ObservableListDelegate(Array(linkLayer,geoNodeLayer).map(_.nodes),panelMapa.children)



  endX.unbind()
  iniY.unbind()

  endX <== convertXView2Real(panelMapa.width)
  iniY <== convertYView2Real(panelMapa.height)




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
    title = "EjeBuilder"
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
