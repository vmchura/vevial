package Layers
import io.vmchura.vevial.EjeVialUtil.UtilFunctions
import Layers.MilestoneLayer.Hito
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.{Point, TPoint}
import io.vmchura.vevial.PlanarGeometric.ProgresiveEje.EfficientEjeProgresiva
import scalafx.scene.Node
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.beans.property.DoubleProperty
import scalafx.geometry.VPos
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, FontSmoothingType, Text, TextAlignment}
import UtilTransformers.PointTransformer
class MilestoneLayer(eje: EfficientEjeProgresiva) extends LayerBuilder[Hito] {

  override def build(pointTransformer: PointTransformer): TLayer[Hito] = new TLayer[Hito]{



    /**
     * update nodes drawn, (x,y) top left corner (u,v) bottom right corner
     *
     */
    override def update(): Unit = {
      applyFactor(pointTransformer.factor.doubleValue(),
        pointTransformer.offsetX(),
        pointTransformer.iniY(),
        pointTransformer.endX(),
        pointTransformer.offsetY())
    }

    override def conversor(e: Hito): Seq[Node] = MilestoneLayer.convertHitoToSeq(e)(pointTransformer)

    val fromProg: Int = (eje.minProg.toInt / 50) * 50
    val toProg: Int = ((eje.maxProg.toInt + 49) / 50) * 50

    private val hitosBy50 = (fromProg to toProg by 50).flatMap(prog => eje.findPointByProgresive(prog).map(point => Hito(point, prog)))
    /**
     *
     * all changes are 1*log(2) to n = f
     */
    private def applyFactor(f: Double, x0Real: Double, y0Real: Double, xnReal: Double, ynReal: Double): Unit = {
      val n = Math.round(Math.log(f) / Math.log(Math.log(2)))
      val m = n match {
        case _ if n <= -7 => 20000
        case _ if n <= -6 => 15000
        case _ if n <= -5 => 10000
        case _ if n <= -4 => 8000
        case _ if n <= -3 => 4000
        case _ if n <= -2 => 2000
        case _ if n <= -1 => 1000
        case _ if n <= 0 => 500
        case _ if n <= 1 => 250
        case _ if n <= 2 => 100
        case _ => 50

      }
      println("Hitos Here")
      val hitosShouldBeShow = hitosBy50.filter(hito => hito.progresiva % m == 0 && {

        val Point(x, y) = hito.pointVector
        x0Real <= x && x <= xnReal && y0Real <= y && y <= ynReal
      }).toSet
      draw(hitosShouldBeShow)
    }
    pointTransformer.offsetX.onChange((_, _, _) => update())
    update()
  }

  override def minimumX: Double = eje.elements.map(_.in.point.x).min

  override def minimumY: Double = eje.elements.map(_.in.point.y).min

  override def maximumX: Double = eje.elements.map(_.in.point.x).max

  override def maximumY: Double = eje.elements.map(_.in.point.y).max
}
object MilestoneLayer {

  private val lengthAsta = 15d
  private val heightFlag = 20d
  private val widthFlag = 90d
  private val heightBar = 2d
  case class Hito(pointVector: TPoint, progresiva: Int)

  private def convertHitoToSeq(hito: Hito)(pointTransformer: PointTransformer): Seq[Node] = {
    import pointTransformer.{DoubleXView, DoubleYView}
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
    xp <== xb.toView_X



    val asta = new Line(){
      startX <== xp
      startY <== yb.toView_Y
      endX <== startX
      endY <== startY-lengthAsta
      strokeWidth = 2
      stroke = Color.Magenta
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
      y <== yb.toView_Y - (+lengthAsta+heightFlag)
      arcHeight = 5
      arcWidth = 5
      width = widthFlag
      height = heightFlag
      fill = Color.Transparent
      strokeWidth = 2
      stroke = Color.Magenta
    }

    val mensaje: Text = new Text(){
      x <== xp
      y <== yb.toView_Y - ( + lengthAsta + heightFlag-heightBar)
      text = UtilFunctions.convertIntToProgresivaString(prog)

      fill = Color.Magenta
      stroke = Color.Magenta
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
