package gym.evialgame
import gym.SimpeEvialGameAI
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point
import scalafx.Includes._
import scalafx.animation.{FadeTransition, KeyFrame, Timeline}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.binding.NumberBinding
import scalafx.beans.property.DoubleProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene._
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import scalafx.scene.paint.{Color, Paint, PhongMaterial}
import scalafx.scene.shape.{Arc, ArcType, Box, Circle, Cylinder, Line, Polygon, Rectangle, Sphere, StrokeType}
import scalafx.scene.text.{Font, Text}
import scalafx.scene.transform.{Rotate, Scale, Transform, Translate}
import scalafx.util.Duration
import javafx.animation.Animation.Status
import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.{BooleanProperty, DoubleProperty}
import scalafx.scene.control.Button
import scalafx.scene.input.KeyCode
import scalafx.scene.paint.{Color, CycleMethod, LinearGradient, Stop}
import scalafx.scene.shape.{Circle, Rectangle}
import scalafx.scene.{Cursor, Group, Scene}

import scala.language.postfixOps
import scala.collection.mutable
import scala.language.postfixOps


object EvialGameView extends JFXApp{
  private final val world = new Group()

  val pointsSource = (1 to 100).flatMap{ x =>
    val y = x*x/1000d
    List(Point(x,y+2.5),Point(x,y-2.5))
  }

  var evialgame = EvialGame(pointsSource,RopeEje(),0)

  val offsetX = DoubleProperty(1024/2d)
  val offsetY = DoubleProperty(768/2d)

  val scaleDP = DoubleProperty(8.0)
  private val scale = new Scale(){
    x <== scaleDP
    y <== scaleDP
    pivotX = 0
    pivotY = 0
  }

  private val translate = new Translate(){
    x <== offsetX
    y <== offsetY
  }


  val bottomY = offsetY + (scaleDP*(768/2d))
  val leftX = offsetX - (scaleDP*(1024/2d))

  def genGridLabelVerticalLine(xp: Int): Text = new Text(){
    x <== offsetX + (scaleDP*xp)
    y <== bottomY
    text = s"$xp"
    font = Font(size=25)
  }
  def genGridLabelHorizontalLine(yp: Int): Text = new Text(){
    x <== offsetY - (scaleDP*yp)
    y <== bottomY
    text = s"$yp"
    font = Font(size=25)
  }
  val evialGameFX = new EvialGameFX(evialgame,offsetX,offsetY,scaleDP)

  def buildScene(): Unit ={
    world.children += evialGameFX.nodesElement
    world.children += buildGrid()

  }

  def buildGrid(): Node = {

    val nodes = (0 to 200 by 10).flatMap{ i=>
      val hl = new Line(){
        startX = -200
        startY = i
        endX = 200
        endY = i
        transforms ++= List(translate,scale)
        strokeWidth <== DoubleProperty(0.1).divide(scaleDP)
      }
      val vl = new Line(){
        startX = i
        startY = -200
        endX = i
        endY = 200
        transforms ++= List(translate,scale)
        strokeWidth <== DoubleProperty(0.1).divide(scaleDP)
      }
      val labels = if(i%10==0){
        List(genGridLabelHorizontalLine(i),genGridLabelVerticalLine(i))
      }else Nil

      List(hl,vl) ++ labels
    }

    new Group(){
      children ++= nodes.map(_.delegate)
    }
  }

  buildScene()
  stage = new PrimaryStage {
    title = "Evial Game"
    width = 1024
    height = 768
    scene = new Scene {
      content = world
    }
    handleMouse(scene(), world)
    handleKey(scene(),world)
  }


  private def handleMouse(scene: Scene, root: Node): Unit = {
    scene.onMousePressed = (me: MouseEvent) => {


    }
  }
  private def handleKey(scene: Scene, root: Node): Unit = {
    scene.onKeyPressed = k => {
      val toUpdate = k.code match {
        case KeyCode.Up => Some(evialgame.forward())
        case KeyCode.Down => Some(evialgame.backward())
        case KeyCode.Right => Some(evialgame.turn(-10))
        case KeyCode.Left => Some(evialgame.turn(10))
        case KeyCode.Numpad0 => {
          val simpleAI = new SimpeEvialGameAI(evialgame)
          println("Solving")
          Some(simpleAI.solve())
        }
        case _ => None
      }

      k.code match {
        case KeyCode.A => offsetX() = offsetX() + scaleDP()*10
        case KeyCode.D => offsetX() = offsetX() - scaleDP()*10
        case KeyCode.W => offsetY() = offsetY() + scaleDP()*10
        case KeyCode.S => offsetY() = offsetY() - scaleDP()*10
        case KeyCode.U => scaleDP() = scaleDP()+1
        case KeyCode.J => scaleDP() = scaleDP()-1
        case _ => ()
      }
      toUpdate.foreach{ev =>
        evialgame = ev
        evialGameFX.updateView(evialgame)
      }



    }
  }


}
