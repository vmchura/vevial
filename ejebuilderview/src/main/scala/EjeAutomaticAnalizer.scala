import java.io.File

import AutomaticBuilder.models.ElementActionToImprove
import Layers._
import UtilTransformers.PointTransformer
import UtilTransformers.PointTransformer._
import algorithms.AssignTangents
import com.typesafe.scalalogging.Logger
import io.DraftManager
import io.vmchura.vevial.EjeVialBuilder.TConvertibleToEje
import io.vmchura.vevial.PlanarGeometric.BasicEje.{EfficientSeqEjeElements, EmptySeqEjeElements, TSeqEjeElementsBase}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, TPoint}
import io.vmchura.vevial.PlanarGeometric.EjeElement.{CircleSegment, FaintElement, RectSegment}
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.{EfficientEjeProgresiva, TEfficientSeqEjeElementsProgresiva}
import io.vmchura.vevial.PlanarGeometric.RestrictiveEje.ProgresivePoint
import io.vmchura.vevial.elementdata.IRIElementData
import io.vmchura.vevial.relevamiento.RelevamientoIRI
import javafx.scene.input
import models._
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.Insets
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, Button}
import scalafx.scene.input.{MouseEvent, TransferMode}
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.{Cursor, Scene}

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.xml.{Elem, XML}

object EjeAutomaticAnalizer extends JFXApp{
  private val logger = Logger(this.getClass)
  private val relevamientosAdded = ListBuffer.empty[RelevamientoIRI[IRIElementData]]
  private val geoNodeLayer = new GeoNodeLayer()
  private val ejeLayer = new SimpleEjeVialLayer()
  private val projectionLayer = new ProjectionPointLayer()
  offsetX() = 0d
  offsetY() = 0d

  private val ejeOpt = ObjectProperty[Option[TEfficientSeqEjeElementsProgresiva]](None)

  def centerWindowAt(p: TPoint): Unit = {
    offsetX() = p.x - (800/2.0)*factor()
    offsetY() = p.y + (640/2.0)*factor()
  }

  ejeOpt.onChange((_,_,newEje) => {
    ejeLayer.clear()
    newEje.foreach{ eje =>
      ejeLayer.addAll(eje.elements)
      eje.elements.headOption.map(_.in.point).foreach(centerWindowAt)
    }
  })


  def showNewRelevamiento(relevamientos: Seq[RelevamientoIRI[IRIElementData]]): Unit = {
    relevamientos.foreach{ relevamiento =>
      relevamientosAdded.append(relevamiento)
    }
    val relevamientosSimples = relevamientos.map(x => new SimpleIRIRelevamientoLayer(x))
    new ObservableListDelegate(relevamientosSimples.toArray.map(_.nodes),panelMapa.children)

  }

  def loadEjeFromXMLFile(file: File): Either[Exception,EfficientEjeProgresiva] = {
    val doc: Elem = XML.loadFile(file)
    val (linkInitial,seqFiles) = DraftManager.loadProject(doc)

    val convertibleEje: TConvertibleToEje = new TConvertibleToEje{
      private val links = linkInitial.untilEnd().flatMap(_.elements).map{
        case RectTemporal(originPoint, endPoint, _) => RectSegment(originPoint,endPoint)
        case CircleTemporal(originPoint, centerPoint, endPoint, antiClockWise, _) => CircleSegment(originPoint,centerPoint,endPoint,antiClockWise)
        case FaintTemporal(from, end, _) => FaintElement(from,end)
      }

      override protected def getSequenceElements: Either[Exception, EfficientSeqEjeElements] =
        try{
          val inefficientEje = links.foldLeft(EmptySeqEjeElements() :TSeqEjeElementsBase)((a,b) => a.append(b))
          Right(EfficientSeqEjeElements(inefficientEje))
        }catch {
          case e: Exception => Left(e)
          case t: Throwable => Left(new IllegalArgumentException(s"with the data cant produce EfficientSeqEjeElements : ${t.toString}"))
        }



      override protected def getSequenceProgresivePoint: Iterable[ProgresivePoint] = links.headOption match {
        case Some(x) => List(new ProgresivePoint(x.in.point,0d))
        case None => Nil
      }
    }

    val res: Either[Exception, TConvertibleToEje] = convertibleEje.toEje.map(efficientEjeProgresiva => {
      AssignTangents.calcEjeWithBasicMath(efficientEjeProgresiva,seqFiles)
    })


    res.flatMap(_.toEje)
  }


  def fileIntoRelevation(file: File): Either[Exception, Unit] = {
    try {
      val ri = RelevamientoIRI(file, cd => IRIElementData(cd))
      val relevamientosSimples = new SimpleIRIRelevamientoLayer(ri)
      new ObservableListDelegate(Array(relevamientosSimples.nodes), panelMapa.children)
      Right()
    }catch{
      case exception: Exception => Left(exception)
      case t: Throwable => Left(new IllegalStateException(s"ERROR: $t"))
    }
  }

  private val panelMapa = new Pane(){

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

        val hasXmlExtension: File => Boolean = {_.getAbsolutePath.endsWith(".xml")}
        val files = db.getFiles.asScala.toList

        val file = files.find(hasXmlExtension) match {
          case Some(docFile) => Right(docFile)
          case None => Left(new IllegalArgumentException("input has not a valid .xml file"))
        }
        val eje = file.flatMap(loadEjeFromXMLFile)
        eje match {
          case Left(error) =>
            new Alert(AlertType.Error, s"error $error"){
              resizable = true
            }.showAndWait()
          case Right(ejeEfficient) => ejeOpt() = Some(ejeEfficient)
        }

        file.flatMap(f => {
          val doc: Elem = XML.loadFile(f)
          val (_,seqFiles) = DraftManager.loadProject(doc)
          seqFiles.map(fileIntoRelevation).reduceLeft((a,b) => a.flatMap(_ => b))
        }) match {
          case Left(error) => new Alert(AlertType.Error, s"error $error"){
            resizable = true
          }.showAndWait()
          case Right(_) => ()
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
    val DragginMap, NotMoving = Value
  }
  import MovingState._

  private var movingState = NotMoving
  private val lastPositionX = new ObjectProperty[Option[Double]](this,"lastPositionX",None)
  private val lastPositionY = new ObjectProperty[Option[Double]](this,"lastPositionY",None)



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

    }

  }

  def getPointFromActionEvent(ae: MouseEvent): Point = {
    val px = PointTransformer.convertXView2Real(ae.getX)
    val py = PointTransformer.convertYView2Real(ae.getY)
    Point(px,py)
  }

  panelMapa.onMouseReleased = ae => {
    stage.scene().cursor = Cursor.Move
    movingState = NotMoving
    lastPositionX() = None
    lastPositionY() = None

  }
  var elementToImprove = Option.empty[ElementActionToImprove]
  panelMapa.onMousePressed = ae => {
    if(ae.isMiddleButtonDown)
      movingState = DragginMap





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
    title = "ResultAfterBuild"
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
