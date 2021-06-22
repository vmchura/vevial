package analizeSource

import com.scalakml.io.KmlPrintWriter
import com.scalakml.kml.{FeaturePart, Folder, HexColor, Kml}

import java.awt.Color
import java.io.File
import scala.util.Random
import scala.xml.PrettyPrinter

case class GraphSourceFiles(
    nodes: Map[String, SourceFile],
    edges: Map[String, List[String]]
) {
  //Segments or tramos where more than 1 point it, but he does not point anything

  val stronglyConnectedComponents: List[List[SourceFile]] =
    StronglyConnectedComponents.buildSCC(nodes, edges)

  val largerNodes: List[SourceFile] = {
    val nodesInArray = Array.fill(nodes.size)(false)
    val nodesIndex = nodes.zipWithIndex.map {
      case ((k, _), indx) => k -> indx
    }.toMap
    edges.flatMap(_._2).foreach { parent =>
      nodesInArray(nodesIndex(parent)) = true
    }
    nodes
      .filter {
        case (id, _) =>
          //has tramos pointing to it && has no tramos pointing out
          nodesInArray(nodesIndex(id)) && edges.getOrElse(id, Nil).isEmpty
      }
      .values
      .toList
  }
  //tramos which points each other

  val ejeSourceFiles: List[EjeSourceFiles] = stronglyConnectedComponents
    .filter { principales =>
      principales.forall { singlePrincipal =>
        edges
          .getOrElse(singlePrincipal.hashID, Nil)
          .forall(principales.map(_.hashID).contains)
      }
    }
    .map { principales =>
      {
        val smallerComponents = nodes.flatMap {
          case (k, v) =>
            Option.when(
              !principales.contains(v) &&
                edges
                  .getOrElse(k, Nil)
                  .exists(eOut => principales.map(_.hashID).contains(eOut))
            )(v)
        }

        EjeSourceFiles(principales, smallerComponents.toList)
      }
    }

  def copyLargeNodes(destination: String): Unit = {
    copyFilesWithProcess(
      destination,
      (sf, parentDirectory) => {

        java.nio.file.Files.copy(
          sf.inputFile.toPath,
          new File(
            parentDirectory,
            sf.inputFile.getName
          ).toPath
        )

      },
      largerNodes
    )
  }
  def copyLargeNodesAsKml(destination: String): Unit = {
    copyFilesWithProcess(
      destination,
      (sf, parentDirectory) => {
        sf.buildEje().map { eje =>
          val fileResult = new File(
            parentDirectory,
            sf.inputFile.getName.replace(".csv", ".kml")
          )
          eje.exportKML(fileResult)
        }

      },
      largerNodes
    )
  }
  def exportPrincipalEjes(destination: String): Unit = {

    def randomColor(): Color = {
      val r: Int = Random.nextInt(255)
      val g: Int = Random.nextInt(255)
      val b: Int = Random.nextInt(255)
      new Color(r, g, b, 255)
    }

    val kmlFolders = ejeSourceFiles
      .filter(_.allSourceFiles.nonEmpty)
      .groupBy(esf =>
        esf.allSourceFiles.find(sf =>
          GraphSourceFiles.fileNameTramo.contains(sf.inputFile.getName)
        ) match {
          case Some(sf) => GraphSourceFiles.fileNameTramo(sf.inputFile.getName)
          case None     => Random.nextString(3)
        }
      )
      .map {
        case (folderName, ejeSources) =>
          val hexColor = HexColor(HexColor.colorToHex(randomColor()))
          val placeMarks =
            ejeSources.flatMap(_.allSourceFiles).flatMap { sf =>
              sf.buildEje()
                .map(eje =>
                  eje.extractPlaceMark(Option(sf.inputFile.getName), hexColor)
                )
            }
          Folder(
            features = placeMarks,
            featurePart = FeaturePart(
              name = Some(folderName)
            )
          )
      }
      .toList
    val folderTramos = Folder(
      features = kmlFolders,
      featurePart = FeaturePart(name = Some("Relevamientos"))
    )
    val kml = Kml(feature = Option(folderTramos))
    prepareDestination(destination)
    new KmlPrintWriter(
      new File(new File(destination), "relevamientos.kml").getPath
    ).write(Option(kml), new PrettyPrinter(80, 3))

  }

  private def prepareDestination(destination: String): Unit = {
    val directoryResult = new File(destination)

    require(
      java.nio.file.Files.isDirectory(
        directoryResult.toPath
      ) || !java.nio.file.Files.exists(directoryResult.toPath),
      "Result of destination should be a directory or not exists"
    )
    if (!java.nio.file.Files.exists(directoryResult.toPath))
      java.nio.file.Files.createDirectory(directoryResult.toPath)

    directoryResult.listFiles().foreach { f =>
      if (java.nio.file.Files.isRegularFile(f.toPath)) {
        java.nio.file.Files.delete(f.toPath)
      }
    }
  }
  def copyFilesWithProcess(
      destination: String,
      process: (SourceFile, File) => Unit,
      files: List[SourceFile]
  ): Unit = {
    val directoryResult = new File(destination)
    files.foreach { sf =>
      println(sf.inputFile.getName)
      process(sf, directoryResult)

    }
  }

  override def toString: String =
    s"Larger: ${largerNodes.mkString("\n")} \n"
}

object GraphSourceFiles {
  val fileNameTramo: Map[String, String] = Map(
    "2019-01-05 14h38m27s Survey.csv" -> "Tramo-I-Aya-Cus-Jun",
    "2019-03-29 08h14m33s Survey.csv" -> "Tramo-II-Aya-Cus-Jun",
    "2019-03-29 07h46m18s Survey.csv" -> "Tramo-II-Aya-Cus-Jun",
    "2019-03-29 10h19m32s Survey.csv" -> "Tramo-II-Aya-Cus-Jun",
    "2019-03-29 07h15m07s Survey.csv" -> "Tramo-II-Aya-Cus-Jun",
    "2019-03-29 10h39m43s Survey.csv" -> "Tramo-II-Aya-Cus-Jun",
    "2018-08-01 13h24m33s Survey.csv" -> "Tramo-III-Aya-Cus-Jun",
    "2018-10-27 08h40m56s Survey.csv" -> "Tramo-III-Appendix-Aya-Cus-Jun",
    "2019-01-31 18h50m54s Survey.csv" -> "Tramo-IV-Aya-Cus-Jun",
    "2019-03-29 15h53m18s Survey.csv" -> "Tramo-V-Aya-Cus-Jun",
    "2020-02-20 11h03m28s Survey T6 HDER.csv" -> "Tramo-VI-Aya-Cus-Jun",
    "2019-03-30 13h49m57s Survey.csv" -> "Tramo-VI-Aya-Cus-Jun",
    "2019-03-30 13h39m28s Survey.csv" -> "Tramo-VI-Aya-Cus-Jun",
    "2019-03-30 13h09m41s Survey.csv" -> "Tramo-VI-Aya-Cus-Jun",
    "2019-01-31 12h16m44s Survey.csv" -> "Tramo-VII-Aya-Cus-Jun",
    "2018-08-29 12h34m26s Survey.csv" -> "Oxapampa-Huancabamba",
    "2019-08-01 11h28m32s Survey.csv" -> "Huancabamba-Pozuzo",
    "2018-10-01 08h45m24s Survey.csv" -> "Pozuzo-CodoPozuzo",
    "2019-08-02 13h09m06s Survey.csv" -> "CodoPozuzo[A]-PuertoInca",
    "2019-04-19 12h59m33s Survey.csv" -> "CodoPozuzo[B]-PuertoInca",
    "2019-05-28 16h08m22s Survey.csv" -> "Churubamba-Chaglla",
    "2019-03-01 10h33m28s Survey.csv" -> "Churubamba-Chaglla",
    "2018-10-29 13h18m39s Survey.csv" -> "Chaglla-Chagroato",
    "2018-07-31 09h56m22s Survey.csv" -> "Prueba-Ignorar",
    "2018-12-27 15h24m54s Survey.csv" -> "Prueba-Ignorar"
  )
}
