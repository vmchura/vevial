package Layers


import io.vmchura.vevial.EjeVialBuilder.LandXMLToEje
import io.vmchura.vevial.EjeVialUtil.UtilFunctions
import UtilTransformers.PointTransformer._
import com.typesafe.scalalogging.Logger
import org.scalatest.FlatSpec
import io.vmchura.vevial.relevamiento.RelevamientoIRI
import scalafx.collections.ObservableBuffer
import scalafx.scene.Node
import UtilFunctions.time
import io.vmchura.vevial.elementdata.IRIElementData

import scala.io.Codec
import scala.reflect.io.File

class ObservableListDelegateTest extends FlatSpec {


  val fileXML = File("/home/vmchura/Documents/001.Projects/vevial/ejevialview/src/test/resources/tramo123.xml")
  val fileCSV = new java.io.File("/home/vmchura/Documents/001.Projects/vevial/relevamientodata/src/test/resources/2019-03-05 14h36m22s Survey.csv")
  val logger = Logger[ObservableListDelegateTest]
  def tlogger[R](prefix: String, block: => R): R = time(logger)(prefix,block)
  val arraySeqNodes: Array[ObservableBuffer[Node]] = {
    val eje =  tlogger("creating landxmleje",new LandXMLToEje(fileXML.reader(Codec("UTF-8"))).toEje)
    eje match {
      case Right(value) =>
        offsetX() = value.elements.head.in.point.x
        offsetY() = value.elements.head.in.point.y
        val relevamientoIRI = tlogger("creating io.vmchura.vevial.relevamiento iri",new SimpleIRIRelevamientoLayer(RelevamientoIRI(fileCSV,cf => IRIElementData(cf))))
        val ejeLayer: EjeVialLayer = new EjeVialLayer(value)
        val milestonesLayer = new MilestoneLayer(value)
        Array(ejeLayer,milestonesLayer,relevamientoIRI).map(_.nodes)
      case _ => Array()
    }
  }
  //val listaChildren = FXCollections.observableArrayList[javafx.scene.Node]()
  //val layersMerged = new ObservableListDelegate(arraySeqNodes,listaChildren)

  behavior of "change if factor"
  it should "work with no errors" in {
    tlogger("changing factor x 2", {


      factor() = factor() * 2

    })
    tlogger("changing offset +1", {


      offsetX() = offsetX() + 1

    })
    tlogger("changing factor x1+1e-6", {


      factor() = factor() * (1+1e-3)

    })
    tlogger("changing offset +1", {


      offsetX() = offsetX() + 1

    })
    tlogger("changing factor x 2", {


      factor() = factor() * 2

    })
    tlogger("changing offset +1", {


      offsetX() = offsetX() + 1

    })
    tlogger("changing factor / 8", {


      factor() = factor() / 8

    })
    tlogger("changing offset +1", {


      offsetX() = offsetX() + 1

    })
    tlogger("changing factor * 8", {


      factor() = factor() + 8

    })
    tlogger("changing offset +1", {


      offsetX() = offsetX() + 1

    })
    tlogger("changing factor / 8", {


      factor() = factor() / 8

    })
    tlogger("changing offset +1", {


      offsetX() = offsetX() + 1

    })
  }


}
