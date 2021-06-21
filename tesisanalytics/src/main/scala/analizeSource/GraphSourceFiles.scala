package analizeSource

import com.scalakml.kml.HexColor

import java.awt.Color
import java.io.File
import scala.util.Random

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
    prepareDestination(destination)

    def randomColor(): Color = {
      val r: Int = Random.nextInt(255)
      val g: Int = Random.nextInt(255)
      val b: Int = Random.nextInt(255)
      new Color(r, g, b, 255)
    }

    ejeSourceFiles.map(_.ejeSourceFiles).foreach { ejeSource =>
      {
        val hexColor = HexColor(HexColor.colorToHex(randomColor()))
        copyFilesWithProcess(
          destination,
          (sf, parentDirectory) => {
            sf.buildEje().map { eje =>
              val fileResult = new File(
                parentDirectory,
                sf.inputFile.getName.replace(".csv", ".kml")
              )
              eje.exportKML(
                fileResult,
                color = hexColor
              )
            }

          },
          ejeSource
        )
      }
    }

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
