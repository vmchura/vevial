import java.io.File

import AutomaticBuilder.models.{ElementActionToImprove, SetPointAt, SimpleAgentEjeEvaluator}
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
import com.typesafe.scalalogging.Logger
import io.DraftManager
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, TPoint}
import io.vmchura.vevial.PlanarGeometric.EjeElement.ElementPoint
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.TEfficientSeqEjeElementsProgresiva
import javafx.scene.input
import models.{EjeEditable, GeoLinkGraph, GeoNode, LinearGraph, LinearGraphEditable, MutableEje, ObserverImpl, TEjeElementTemporal, TLinkPoint}
import scalafx.beans.property.ObjectProperty
import scalafx.event.ActionEvent
import scalafx.geometry.Insets
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.input.{MouseEvent, TransferMode}
import scalafx.scene.paint.Color

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, XML}

object EjeBuilder extends JFXApp{
  val logger = Logger(this.getClass)
  val relevamientosAdded = ListBuffer.empty[RelevamientoIRI[IRIElementData]]
  val filedAdded = ListBuffer.empty[java.io.File]
  val geoNodeLayer = new GeoNodeLayer()
  val ejeLayer = new SimpleEjeVialLayer()
  val projectionLayer = new ProjectionPointLayer()
  offsetX() = 0d
  offsetY() = 0d

  var ejeEditableOpt = Option.empty[EjeEditable]


  class LinkSelector

  def showNewRelevamiento(relevamientos: Seq[RelevamientoIRI[IRIElementData]]): Unit = {
    relevamientos.foreach{ relevamiento =>
      relevamientosAdded.append(relevamiento)
    }
    val relevamientosSimples = relevamientos.map(x => new SimpleIRIRelevamientoLayer(x))
    new ObservableListDelegate(relevamientosSimples.toArray.map(_.nodes),panelMapa.children)

  }

  def generateNewEje(dataEntry: Either[List[RelevamientoIRI[IRIElementData]],TLinkPoint]): Option[EjeEditable] = {
    ejeEditableOpt.foreach{_.clear()}
    ejeEditableOpt = None

    val commonData: (GeoNodeLayer,SimpleEjeVialLayer,TPoint => Unit) = (geoNodeLayer,ejeLayer, (p: TPoint) => {
      offsetX() = p.x - (800/2.0)*factor()
      offsetY() = p.y + (640/2.0)*factor()
    })

    try {
      val eje = dataEntry match {
        case Left(relevamientos) =>
          val nodeEje: Seq[LinearGraph[GeoNode]] = DiscreteRelevamiento.convertIntoDiscreteRelevamiento[RelevamientoIRI[IRIElementData], IRIElementData, GeoNode](relevamientos)
          val singleLinearEje = LinearGraph.mergeLinearGraphs(nodeEje)
          EjeEditable(singleLinearEje)(commonData._1, commonData._2, commonData._3)
        case Right(head) =>
          val links = head.untilEnd()
          EjeEditable(links.flatMap(_.elements), links)(commonData._1, commonData._2, commonData._3)

      }
      eje.setInitialPointsFree(relevamientosAdded.flatMap(_.elements.flatMap(_.point.map(_.value))))
      commonData._3(eje.initialEjeElements.head.in.point)
      Some(eje)
    }catch {
      case e: Throwable =>
        logger.debug(s"No se pudo crear eje: ${e.toString}")
        None
    }
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

    def filesIntoRelevamientos(files: Seq[File]): Seq[RelevamientoIRI[IRIElementData]] = {
      files.flatMap { file: File =>

        try {
          Some(RelevamientoIRI(file, cd => IRIElementData(cd)))
        } catch {
          case e: Throwable => {

            val a = new Alert(AlertType.Warning, s"Cant load file ${file} error: ${e.toString}")
            a.resizable = true
            a.showAndWait()
            None
          }
        }
      }
    }
    onDragDropped = e => {

      val db = e.getDragboard
      if(db.hasFiles){

        val hasXmlExtension: File => Boolean = {_.getAbsolutePath.endsWith(".xml")}
        val files = db.getFiles.asScala.toList
        files.filterNot(hasXmlExtension).foreach(filedAdded.append)

        if(files.exists(hasXmlExtension)){
            val docFile = files.find(hasXmlExtension).get
            val doc: Elem = XML.loadFile(docFile)
            val (linkInitial,seqFiles) = DraftManager.loadProject(doc)
            val relevamientos = filesIntoRelevamientos(seqFiles)
            seqFiles.foreach(filedAdded.append)
            showNewRelevamiento(relevamientos)

            ejeEditableOpt = generateNewEje(Right(linkInitial))

        }else {
          val relevamientos = filesIntoRelevamientos(db.getFiles.asScala.toList)
          showNewRelevamiento(relevamientos)
          ejeEditableOpt = generateNewEje(Left(relevamientosAdded.toList))
        }


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
    val DragginMap, DragginNode,SelectionSquare, NotMoving = Value
  }
  import MovingState._

  var movingState = NotMoving
  var nodeMoving = Option.empty[GeoNode]
  val lastPositionX = new ObjectProperty[Option[Double]](this,"lastPositionX",None)
  val lastPositionY = new ObjectProperty[Option[Double]](this,"lastPositionY",None)

  var startSquare = Option.empty[Point]
  var endSquare = Option.empty[Point]

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

      }else{
        if(movingState == SelectionSquare){

        }else{

        }
      }
    }

  }

  def getPointFromActionEvent(ae: MouseEvent): Point = {
    val px = PointTransformer.convertXView2Real(ae.getX)
    val py = PointTransformer.convertYView2Real(ae.getY)
    Point(px,py)
  }

  panelMapa.onMouseReleased = ae => {

    logger.debug(s"MOUSE RELEASED: state: $movingState")
    if(movingState == DragginNode){
      for{
        node <- nodeMoving
        lg <- ejeEditableOpt
      }yield{

        val point = getPointFromActionEvent(ae)
        lg.moveGeoNodeTo(node,point)
      }
    }else{
      if(movingState == SelectionSquare){
        endSquare = Some(getPointFromActionEvent(ae))
        for{
          start <- startSquare
          end <- endSquare
          eje <- ejeEditableOpt
        }yield {
          logger.debug(s"start: $start - $end")

          val minX = Math.min(start.x,end.x)
          val maxX = Math.max(start.x,end.x)

          val minY = Math.min(start.y,end.y)
          val maxY = Math.max(start.y,end.y)

          logger.debug(f"[$minX%.0f.$maxX%.0f] - [$minY%.0f.$maxY%.0f]")
          logger.debug(s"Length presents: ${eje.geoNodesPresent.length}")
          eje.geoNodesPresent.filter{ p =>
            minX <= p.x && p.x <= maxX &&
            minY <= p.y && p.y <= maxY
          }.foreach(eje.dropNode)



        }
      }
    }

    startSquare = None
    endSquare = None

    stage.scene().cursor = Cursor.Move
    movingState = NotMoving
    lastPositionX() = None
    lastPositionY() = None

  }
  var elementToImprove = Option.empty[ElementActionToImprove]
  panelMapa.onMousePressed = ae => {
    if(ae.isSecondaryButtonDown){
      /*
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

       */
      val point = getPointFromActionEvent(ae)
      ejeEditableOpt.foreach(lp => {
        lp.elementByPosition(point) match {

          case Some(Right(ejeElement)) => {


            ejeElement.ejeElementOwner match {
              case temporal: TEjeElementTemporal =>



                temporal.ejeSection match {
                  case geoPoint: GeoLinkGraph =>

                    projectionLayer.clear()

                    for {
                      eje <- ejeEditableOpt
                    } yield {
                      val ep = geoPoint.pointsDataCovering.flatMap { x =>
                        eje.elementByPosition(x) match {
                          case Some(Right(ep)) => Some(ep)
                          case _ => None
                        }
                      }
                      ep.foreach { e =>
                        projectionLayer.add((e, false))
                      }

                      val observer = new ObserverImpl(geoPoint)
                      geoPoint.pointsDataCovering.foreach(observer.addProjection)
                      val eat = ElementActionToImprove(geoPoint,SimpleAgentEjeEvaluator.deliberateAnAction(observer))
                      logger.debug(s"eat: $eat")
                      eje.applyUpgrade(eat)
                    }
                  case _ => (println("section no geolinkgraph"))
                }
              case z => println(s"NOT temporal $z")
            }


            stage.scene.value.cursor = Cursor.Crosshair
          }
          case Some(Left(node)) => stage.scene.value.cursor = Cursor.OpenHand
          case None => stage.scene.value.cursor = Cursor.Move
        }
      })

    }else{
      if(ae.isMiddleButtonDown){

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
      }else{

        if(ae.isPrimaryButtonDown){
          startSquare = Some(getPointFromActionEvent(ae))
          movingState = SelectionSquare
          logger.debug(s"START: $startSquare")
        }else{

        }
      }


    }


  }

  panelMapa.onMouseMoved = ae => {

    if(movingState == NotMoving) {
    }
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
            val urlSave = "/home/vmchura/Documents/testProject.xml"
            val res = (for{
              eje <- ejeEditableOpt
              head <- eje.headLink()
            }yield{
              Right(DraftManager.saveProject(urlSave)(head,filedAdded.toSeq))
            }).getOrElse(Left("No eje defined"))

            val alert = res match {
              case Left(error) => new Alert(AlertType.Error, s"error $error")
              case Right(true) => new Alert(AlertType.Confirmation, s"Guardado on $urlSave")
              case Right(false) => new Alert(AlertType.Warning, s"No guardado")
            }
            alert.resizable = true
            alert.showAndWait()

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
