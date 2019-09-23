package Layers
import EjeVialUtil.UtilFunctions
import Layers.MilestoneLayer.Hito
import PlanarGeometric.BasicGeometry.Point
import PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import scalafx.scene.Node
import scalafx.scene.shape.{Line, Rectangle}
import UtilTransformers.PointTransformer._
import scalafx.beans.property.DoubleProperty
import scalafx.geometry.VPos
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, FontSmoothingType, Text, TextAlignment}

class MilestoneLayer(eje: EfficientEjeProgresiva) extends TLayer[Hito] {




  /**
    * update nodes drawn, (x,y) top left corner (u,v) bottom right corner

    */
  override def setListenerPanelUpdate(x: DoubleProperty, y: DoubleProperty, u: DoubleProperty, v: DoubleProperty): Unit = ()

  override def conversor(e: Hito): Seq[Node] = MilestoneLayer.convertHitoToSeq(e)

  val fromProg: Int = (eje.minProg.toInt/50)*50
  val toProg: Int = ((eje.maxProg.toInt+49)/50)*50
  private val hitosBy50 = (fromProg to toProg by 50).flatMap(prog => eje.findPointByProgresive(prog).map(point => Hito(point,prog)))


  /**
    *
    * all changes are 1*log(2) to n = f
    * @param f
    */
  private def applyFactor(f: Double,x0Real: Double, y0Real: Double, xnReal: Double, ynReal: Double): Unit = {
    //println(s"$factor  ${xnReal-x0Real}, ${ynReal-y0Real}")
    val n = Math.round(Math.log(f)/Math.log(Math.log(2)))
    // m: module to use to filter
    val m = n match {
      case _ if n <= -7 =>100000
      case _ if n <= -6 =>50000
      case _ if n <= -5 =>20000
      case _ if n <= -4 =>10000
      case _ if n <= -3 => 5000
      case _ if n <= -2 => 2000
      case _ if n <= -1 => 1000
      case _ if n <= 0 => 500
      case _ if n <= 1 => 200
      case _ if n <= 2 => 100
      case _  => 50

    }
    val hitosShouldBeShow = hitosBy50.filter(hito => hito.progresiva % m == 0 && {
      val Point(x,y) = hito.pointVector
      x0Real <= x && x <= xnReal && y0Real <= y && y <= ynReal
    }).toSet
    val hitosShouldBeDeleted = elementsDrawn().diff(hitosShouldBeShow)
    val hitosShouldBeAdded = hitosShouldBeShow.diff(elementsDrawn())
    removeAll(hitosShouldBeDeleted)
    addAll(hitosShouldBeAdded)
  }
  //factor.onChange((_,_,nf) =>applyFactor(nf.doubleValue(),offsetX(),iniY(),endX(),offsetY()))
  offsetX.onChange((_,_,noffset) =>applyFactor(factor(),noffset.doubleValue(),iniY(),endX(),offsetY()))

  applyFactor(factor.doubleValue(),offsetX(),iniY(),endX(),offsetY())
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
      strokeWidth = 2

    }
    /*
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

     */

    val rectangleFlag = new Rectangle(){
      x <== xp
      y <== yb.toView_Y() - (+lengthAsta+heightFlag)
      arcHeight = 5
      arcWidth = 5
      width = widthFlag
      height = heightFlag
      fill = Color.Transparent
      strokeWidth = 2
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
      fontSmoothingType = FontSmoothingType.LCD
    }

    List(asta,
      //rectangleBottom,rectangleTop,
      rectangleFlag,mensaje)
  }
}
