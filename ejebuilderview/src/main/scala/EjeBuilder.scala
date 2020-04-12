import Layers.{EjeVialLayer, InitialDraftLayer, ObservableListDelegate, SimpleIRIRelevamientoLayer}
import UtilTransformers.PointTransformer
import io.vmchura.vevial.elementdata.IRIElementData
import io.vmchura.vevial.relevamiento.RelevamientoIRI
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.collections.ObservableBuffer
import scalafx.scene.{Node, Scene}
import scalafx.scene.control.Button
import scalafx.scene.layout.{BorderPane, Pane}
import UtilTransformers.PointTransformer._
import algorithms.{DiscreteRelevamiento, EjeBuilderDraft}
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point
import models.GeoNode
import scalafx.beans.property.ObjectProperty

object EjeBuilder extends JFXApp{

  val relevamientoFile = new java.io.File("/home/vmchura/Documents/003.CVSC/IRI/Auomated/2020-02-21 12h50m20s Survey T1 HIZQ.csv")
  val relevamientoFile2 = new java.io.File("/home/vmchura/Documents/003.CVSC/IRI/Auomated/2020-02-21 12h13m18s Survey T1 HDER.csv")

  val arraySeqNodes: Array[ObservableBuffer[Node]] = {
    val relevamientoIRI = RelevamientoIRI(relevamientoFile,cd => IRIElementData(cd))
    val relevamientoIRI2 = RelevamientoIRI(relevamientoFile2,cd => IRIElementData(cd))

    val ejeBuilder = new EjeBuilderDraft[RelevamientoIRI[IRIElementData],IRIElementData](
      List(relevamientoIRI,relevamientoIRI2))
    val eje = ejeBuilder.buildEje()
    val ejeLayer: EjeVialLayer = new EjeVialLayer(eje)

    val bosquejoEje = DiscreteRelevamiento.convertIntoDiscreteRelevamiento[RelevamientoIRI[IRIElementData],IRIElementData,GeoNode](
      List(relevamientoIRI,relevamientoIRI2))

    val initialDraftLayer: Seq[InitialDraftLayer] = bosquejoEje.map(x => new InitialDraftLayer(x) )

    println(s"nodes: ${initialDraftLayer.length}")
    val simpleRelevamientoLayer = new SimpleIRIRelevamientoLayer(relevamientoIRI)
    val simpleRelevamientoLaye2r = new SimpleIRIRelevamientoLayer(relevamientoIRI2)
    val somePoint: Point = relevamientoIRI.elements.find(_.point.isDefined).map(_.point.map(_.value).get).get

    offsetX() = somePoint.x
    offsetY() = somePoint.y
    (Array(simpleRelevamientoLayer,simpleRelevamientoLaye2r,ejeLayer) ++ initialDraftLayer.toArray).map(_.nodes)
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
