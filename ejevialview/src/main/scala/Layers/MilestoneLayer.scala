package Layers
import EjeVialUtil.UtilFunctions
import Layers.MilestoneLayer.Hito
import PlanarGeometric.BasicGeometry.{Point, PointUnitaryVector}
import PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import scalafx.collections.ObservableBuffer
import scalafx.scene.Node
import scalafx.scene.shape.{Line, Rectangle}
import UtilTransformers.PointTransformer._
import scalafx.beans.property.DoubleProperty
import scalafx.geometry.VPos
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, Text, TextAlignment}

class MilestoneLayer(eje: EfficientEjeProgresiva) extends TLayer[Hito] {




  /**
    * update nodes drawn, (x,y) top left corner (u,v) bottom right corner
    *
    * @param x
    * @param y
    * @param u
    * @param v
    */
  override def setListenerPanelUpdate(x: DoubleProperty, y: DoubleProperty, u: DoubleProperty, v: DoubleProperty): Unit = ()

  override def conversor(e: Hito): Seq[Node] = MilestoneLayer.convertHitoToSeq(e)

  val fromProg: Int = (eje.minProg.toInt/50)*50
  val toProg: Int = ((eje.maxProg.toInt+49)/50)*50
  println(s"$fromProg => $toProg")
  val hitosBy50 = (fromProg to toProg by 50).flatMap(prog => eje.findPointByProgresive(prog).map(point => Hito(point,prog)))
  println(s"|hitos| ${hitosBy50.length}")
  addAll(hitosBy50)

}
object MilestoneLayer {

  private val lengthAsta = 15d
  private val heightFlag = 20d
  private val widthFlag = 90d
  private val heightBar = 2d
  case class Hito(pointVector: Point, progresiva: Int)
  import UtilTransformers.PointTransformer._
  def convertHitoToSeq(hito: Hito): Seq[Node] = {
    val Hito(Point(xb,yb), prog) = hito
    /**
      * |============|
      * |   0+000    |
      * |============|
      * |
      * |
      * |
      *
      */

    // x de hasta
    val xp = new DoubleProperty()
    xp <== xb.toView_X()



    val asta = new Line(){
      startX <== xp
      startY <== yb.toView_Y()
      endX <== startX
      endY <== startY-lengthAsta
      strokeWidth = 5

    }
    val rectangleTop = new Rectangle(){
      x <== xp
      y <== yb.toView_Y() - (+lengthAsta+heightFlag)
      arcHeight = 5
      arcWidth = 5
      width = widthFlag
      height = heightBar
      fill = Color.Black
      strokeWidth = 1
    }
    val rectangleBottom = new Rectangle(){
      x <== xp
      y <== yb.toView_Y() - (+lengthAsta+heightBar)
      arcHeight = 5
      arcWidth = 5
      width = widthFlag
      height = heightBar
      fill = Color.Black
      strokeWidth = 1
    }

    val rectangleFlag = new Rectangle(){
      x <== xp
      y <== yb.toView_Y() - (+lengthAsta+heightFlag)
      arcHeight = 5
      arcWidth = 5
      width = widthFlag
      height = heightFlag
      fill = Color.Transparent
      strokeWidth = 5
      stroke = Color.Black
    }

    val mensaje: Text = new Text(){
      x <== xp
      y <== yb.toView_Y() - ( + lengthAsta + heightFlag-heightBar)
      text = UtilFunctions.convertIntToProgresivaString(prog)

      fill = Color.Black
      stroke = Color.Black
      textAlignment = TextAlignment.Center
      wrappingWidth = widthFlag
      textOrigin = VPos.Top
      font = new Font(12)
    }

    List(asta,rectangleBottom,rectangleTop,rectangleFlag,mensaje)
  }
}
