package io.vmchura.vevial.relevamiento

import io.vmchura.vevial.EjeVialBuilder.LandXMLToEje

import scala.reflect.io.File
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Codec
class SurveyGPXTest extends AnyFlatSpec {
  behavior of "Building io.vmchura.vevial.relevamiento GPX"
  val existCSVFile = false
  it should "end with no errors" in {
    assume(existCSVFile)
    val gpxFilePath = getClass.getClassLoader.getResource("gpxFile.gpx").getPath
    val roadAxisPath = getClass.getClassLoader.getResource("roadAxis.xml").getPath

    val roadAxisEither = new LandXMLToEje(File(roadAxisPath).reader(Codec("UTF-8"))).toEje
    assume(roadAxisEither.isRight)
    roadAxisEither.foreach{ roadAxis =>
      val surveyEither = SurveyGPX(gpxFilePath)
      assert(surveyEither.isRight)
      surveyEither.foreach{ survey =>
        val elementsPoints = survey.surveyInformation.map(gpxElementData =>
          (gpxElementData.point.map(_.value).flatMap(roadAxis.projectPoint),
            gpxElementData.point.map(_.value),
            gpxElementData.originalData))

        val totalElements = elementsPoints.length
        println(s"total elements $totalElements")

        val (projectedElementsOption, noProjectedElementsOption) = elementsPoints.span(_._1.isDefined)
        println(s"total projected/noprojected ${projectedElementsOption.length}/${noProjectedElementsOption.length}")
        val projectedElements = projectedElementsOption.map{
          case (Some(elementPoint), point, originalData) => (elementPoint, point, originalData)
          case _ => throw new IllegalStateException("not valid state")
        }
        val noProjectedElements = noProjectedElementsOption.map{case (_, point, originalData) => (point, originalData)}

        val lengthsProjection = projectedElements.map(_._1.lengthToPointAbs)
        println(s"lengths: min:${lengthsProjection.min}/max:${lengthsProjection.min}")
        val horizontalWithProjection = projectedElements.flatMap(_._2.map(_.x))
        println(s"horizontal projected : min:${horizontalWithProjection.min}/max:${horizontalWithProjection.max}")
        val verticalWithProjection = projectedElements.flatMap(_._2.map(_.y))
        println(s"vertical projected : min:${verticalWithProjection.min}/max:${verticalWithProjection.max}")
        val horizontalNoProjection = noProjectedElements.flatMap(_._1.map(_.x))
        val verticalNoProjection = noProjectedElements.flatMap(_._1.map(_.y))
        println(s"horizontal no projected: min:${horizontalNoProjection.min}/max:${horizontalNoProjection.max}")
        println(s"vertical no projected: min:${verticalNoProjection.min}/max:${verticalNoProjection.max}")
        println("Nodes with no points")
        noProjectedElements.filter(_._1.isEmpty).map(_._2).foreach(println)
        println("x higher than 9000000")
        noProjectedElements.filter{
          case (Some(point), _) if point.x > 9000000 => true
          case _ => false
        }.map(_._2).foreach(println)

      }
    }

  }

}