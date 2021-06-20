package analizeSource

import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import java.nio.file.Path

class TEfficientSeqEjeElementsTest extends AnyFlatSpec {
  behavior of "TEfficientSeqEjeElements"
  it should "export KML from relevamiento" in {
    def buildKmlFromCSV(originPath: String, destinationPath: String): Unit =
      SourceFile(new File(originPath))
        .buildEje()
        .foreach(_.exportKML(destinationPath))

    buildKmlFromCSV(
      "/home/vmchura/Documents/003.Tesis/DataSource/20m/E/2020-02-20 16h17m08s Survey T5 HIZQ.csv",
      "/home/vmchura/Documents/003.Tesis/DataSource/2020-02-20 16h17m08s Survey T5 HIZQ.kml"
    )
    buildKmlFromCSV(
      "/home/vmchura/Documents/003.Tesis/DataSource/20m/E/2019-06-27 09h33m55s Survey.csv",
      "/home/vmchura/Documents/003.Tesis/DataSource/2019-06-27 09h33m55s Survey.kml"
    )
    assert(
      java.nio.file.Files.exists(
        Path.of(
          "/home/vmchura/Documents/003.Tesis/DataSource/2018-10-29 16h07m16s Survey.kml"
        )
      )
    )
  }
}
