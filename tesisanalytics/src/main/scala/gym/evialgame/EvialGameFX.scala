package gym.evialgame
import io.vmchura.vevial.PlanarGeometric.BasicGeometry.Point

import scala.collection.JavaConverters._
import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty, StringProperty}
import scalafx.scene.{Group, Node}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line}
import scalafx.scene.text.{Font, Text}
import scalafx.scene.transform.{Scale, Translate}

class EvialGameFX(evialGame: EvialGame,offsetX: DoubleProperty, offsetY: DoubleProperty, scaleDP: DoubleProperty) {
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

  private val translations = evialGame.ropeEje.nodes.map(rn => (DoubleProperty(rn.originalPoint.x),DoubleProperty(rn.originalPoint.y)))

  def scoreToString(scoreDouble: Double): String = f"$scoreDouble%6.2f"
  val scoreProperty = StringProperty("-")
  private val score = new Text(){
    text <== scoreProperty
    x = 0
    y = 50
    font = Font(size = 25)
  }

  private val pointsGroup = new Group(){
    children ++= buildDataSourceGroup().map(_.delegate)
  }
  private val ropeGroup = new Group(){
    children ++= buildRopeGroup().map(_.delegate)
  }
  private val sampleGroup = new Group()

  val nodesElement = new Group(){
    children ++= List(pointsGroup,ropeGroup, score,sampleGroup)
  }

  private val n = RopeEje().nodes.length
  private def buildRopeGroup(): Seq[Node] ={
    val circles = translations.map{ case (x,y) =>
      new Circle(){
        centerX <== x
        centerY <== y
        radius = 0.1
        fill = Color.Brown
        transforms ++= List(translate,scale)
      }
    }
    val links = translations.zip(translations.tail).map{ case ((x0,y0),(x1,y1)) =>
      new Line(){
        startX <== x0
        startY <== y0
        endX <== x1
        endY <== y1
        stroke = Color.Brown
        strokeWidth = 0.01
        transforms ++= List(translate,scale)
      }
    }
    (circles ++ links)
  }
  def updateView(evialGame: EvialGame): Unit = {
    evialGame.ropeEje.nodes.zip(translations).foreach{ case (rp,(x,y)) =>
      x() = rp.originalPoint.x
      y() = rp.originalPoint.y
    }
    sampleGroup.children.clear()
    if(!evialGame.gameDone){
      val samples = evialGame.sampleState()
      val nodes: Seq[Node] = samples.flatMap{
        case SampleVector(_,distance,vec) => Some(new Line(){
          startX = vec.point.x
          startY = vec.point.y
          endX = (vec.point + vec.planarVector).x
          endY = (vec.point + vec.planarVector).y
          strokeWidth = 0.5
          stroke = if(distance>0) Color.GreenYellow else Color.BlueViolet
          transforms ++= List(translate,scale)
        })
        case _ => None

      }
      sampleGroup.children.addAll(nodes.map(_.delegate).asJava)
    }
    scoreProperty() = scoreToString(evialGame.currentScore)
  }

  private def buildDataSourceGroup(): Seq[Node] = {
    evialGame.parameters.map{ case Point(x,y) =>
      new Circle(){
        centerX <== x
        centerY <== y
        radius = 0.4
        fill = Color.IndianRed
        transforms ++= List(translate,scale)
      }
    }
  }

  updateView(evialGame)
}
