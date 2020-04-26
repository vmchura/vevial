import AutomaticBuilder.models.{ElementActionToImprove, SetPointAt}
import scalafx.Includes._
import Layers.{GeoNodeLayer, InitialDraftLayer, LinkLayer, ObservableListDelegate, ProjectionPointLayer, SimpleEjeVialLayer, SimpleIRIRelevamientoLayer}
import UtilTransformers.PointTransformer
import io.vmchura.vevial.elementdata.IRIElementData
import io.vmchura.vevial.relevamiento.RelevamientoIRI
import scalafx.application.{JFXApp, Platform}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.collections.ObservableBuffer
import scalafx.scene.{Cursor, Node, Scene}
import scalafx.scene.control.{Alert, Button}
import scalafx.scene.layout.{Background, BackgroundFill, BorderPane, CornerRadii, Pane}
import UtilTransformers.PointTransformer._
import algorithms.{DiscreteRelevamiento, EjeBuilderDraft}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point
import io.vmchura.vevial.PlanarGeometric.EjeElement.ElementPoint
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.TEfficientSeqEjeElementsProgresiva
import javafx.scene.input
import models.{EjeEditable, GeoLinkGraph, GeoNode, LinearGraph, LinearGraphEditable, MutableEje, TEjeElementTemporal}
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.Insets
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.input.TransferMode
import scalafx.scene.paint.Color

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer

object EjeBuilder extends JFXApp{

  val relevamientosAdded = ListBuffer.empty[RelevamientoIRI[IRIElementData]]
  val geoNodeLayer = new GeoNodeLayer()
  val ejeLayer = new SimpleEjeVialLayer()
  val projectionLayer = new ProjectionPointLayer()
  offsetX() = 0d
  offsetY() = 0d

  var ejeEditableOpt = Option.empty[EjeEditable]


  class LinkSelector


  def loadNewFile(relevamientos: Seq[RelevamientoIRI[IRIElementData]]): Unit = {
    relevamientos.foreach{ relevamiento =>
      relevamientosAdded.append(relevamiento)
    }
    val relevamientosSimples = relevamientos.map(x => new SimpleIRIRelevamientoLayer(x))

    ejeEditableOpt.foreach{_.clear()}
    ejeEditableOpt = None


    val nodeEje: Seq[LinearGraph[GeoNode]] = DiscreteRelevamiento.convertIntoDiscreteRelevamiento[RelevamientoIRI[IRIElementData],IRIElementData,GeoNode](relevamientosAdded.toList)

    val singleLinearEje = LinearGraph.mergeLinearGraphs(nodeEje)

    ejeEditableOpt = Some(EjeEditable(singleLinearEje,geoNodeLayer,ejeLayer, p => {
      offsetX() = p.x - (800/2.0)*factor()
      offsetY() = p.y + (640/2.0)*factor()
    }))
    ejeEditableOpt.foreach(_.setInitialPointsFree(relevamientos.flatMap(_.elements.flatMap(_.point.map(_.value)))))

    offsetX() = singleLinearEje.nodes.head.center.x
    offsetY() = singleLinearEje.nodes.head.center.y


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


  new ObservableListDelegate(Array(geoNodeLayer,ejeLayer,projectionLayer).map(_.nodes),panelMapa.children)



  endX.unbind()
  iniY.unbind()

  endX <== convertXView2Real(panelMapa.width)
  iniY <== convertYView2Real(panelMapa.height)
  object MovingState extends Enumeration {
    type MovingState = Value
    val DragginMap, DragginNode, NotMoving = Value
  }
  import MovingState._

  var movingState = NotMoving
  var nodeMoving = Option.empty[GeoNode]
  val lastPositionX = new ObjectProperty[Option[Double]](this,"lastPositionX",None)
  val lastPositionY = new ObjectProperty[Option[Double]](this,"lastPositionY",None)
  panelMapa.onMouseDragged = ae => {

    if(movingState == DragginMap) {


      for {
        lx <- lastPositionX()
        ly <- lastPositionY()
      } yield {
        offsetX() = offsetX() - (ae.getX - lx) * factor()
        offsetY() = offsetY() + (ae.getY - ly) * factor()
      }


      lastPositionX() = Some(ae.getX)
      lastPositionY() = Some(ae.getY)

    }else{
      if(movingState == DragginNode){

      }
    }

  }

  panelMapa.onMouseReleased = ae => {

    if(movingState == DragginNode){
      for{
        node <- nodeMoving
        lg <- ejeEditableOpt
      }yield{
        val px = PointTransformer.convertXView2Real(ae.getX)
        val py = PointTransformer.convertYView2Real(ae.getY)
        val point = Point(px,py)
        lg.moveGeoNodeTo(node,point)
      }
    }

    stage.scene().cursor = Cursor.Move
    movingState = NotMoving
    lastPositionX() = None
    lastPositionY() = None

    println("Mouse released")
  }
  var elementToImprove = Option.empty[ElementActionToImprove]
  panelMapa.onMousePressed = ae => {
    if(ae.isSecondaryButtonDown){
      if(elementToImprove.isEmpty){
        ejeEditableOpt.foreach(ej => {
          ej.popNextUpgrade() match {
            case Some(up) => {

              elementToImprove = Some(up)
              ej.locateUpgrade(up.elementCanImprove)

              val elementToDraw: Option[ElementPoint] = up.actionImproveEje match {
                case e: SetPointAt =>
                  (for{
                    //FIXME it should inside de link, not a projection
                    point <- up.elementCanImprove.calcPointFromProjection(e)
                    eje <- ejeEditableOpt
                    ep <- eje.elementByPosition(point)
                  } yield{
                    ep match {
                      case Left(_) => None
                      case Right(x) => Some(x)
                    }
                  }).flatten

                case _ => None
              }


/*
              projectionLayer.clear()
              up.elementCanImprove match {
                case geoPoint: GeoLinkGraph =>
                  for{
                    eje <- ejeEditableOpt
                  }yield{
                    val ep = geoPoint.pointsDataCovering.flatMap { x =>
                      eje.elementByPosition(x) match {
                        case Some(Right(ep)) => Some(ep)
                        case _ => None
                      }
                    }
                    ep.foreach{ e =>
                      projectionLayer.add((e,false))
                    }
                  }
                case _ => ()
              }
*/
              elementToDraw.foreach(e => projectionLayer.add((e,true)))


            }
            case None => println("nothing to upgrade")
          }
        })
      }else{
        ejeEditableOpt.foreach(ej => {
          if (ej.applyUpgrade(elementToImprove.get)) {
            println("change made")
          } else {
            println("cant do change")
          }
        })

        elementToImprove = None
      }

    }else{

      val px = PointTransformer.convertXView2Real(ae.getX)
      val py = PointTransformer.convertYView2Real(ae.getY)
      val point = Point(px,py)
      val newMovingState = for{
        lg <- ejeEditableOpt
        res <- lg.elementByPosition(point)
      }yield{
        res match {
          case Right(ep) =>{
            val node = lg.createInnerNode(ep)
            stage.scene().cursor = Cursor.ClosedHand
            nodeMoving = Some(node)
            DragginNode
          }
          case Left(node) => {
            stage.scene().cursor = Cursor.ClosedHand
            nodeMoving = Some(node)
            DragginNode
          }
        }
      }
      newMovingState match {
        case Some(ms) => movingState = ms
        case None => movingState = DragginMap
      }
    }


  }

  panelMapa.onMouseMoved = ae => {
    val px = PointTransformer.convertXView2Real(ae.getX)
    val py = PointTransformer.convertYView2Real(ae.getY)
    val point = Point(px,py)
    ejeEditableOpt.foreach(lp => {
      lp.elementByPosition(point) match {

        case Some(Right(ejeElement)) => {


              ejeElement.ejeElementOwner match {
                case temporal: TEjeElementTemporal =>


                  temporal.ejeSection match {
                    case geoPoint: GeoLinkGraph =>
                      projectionLayer.clear()

                      for{
                        eje <- ejeEditableOpt
                      }yield{
                        val ep = geoPoint.pointsDataCovering.flatMap { x =>
                          eje.elementByPosition(x) match {
                            case Some(Right(ep)) => Some(ep)
                            case _ => None
                          }
                        }
                        ep.foreach{ e =>
                          projectionLayer.add((e,false))
                        }
                      }
                    case _ => (println("section no geolinkgraph"))
                  }
                case z => println(s"NOT temporal $z")
              }


          stage.scene.value.cursor = Cursor.Crosshair
        }
        case Some(Left(node)) =>  stage.scene.value.cursor = Cursor.OpenHand
        case None => stage.scene.value.cursor = Cursor.Move
      }
    })
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
        top = new Button("top"){
          onAction = _ => {


          }
        }
        left = new Button("left"){
          onAction = _ => {
            println("click on top!!")
          }
        }
        center =  panelMapa
      }
    }
  }
}
