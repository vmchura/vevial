package CommunicationLayerPython

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.Socket

import org.scalatest.FlatSpec

class GymServerTest extends FlatSpec {

  behavior of "Creation of GymServer"

  it should " build correctly " in {
    val gym = new GymServer(8084)
    gym.start()

  }


}
