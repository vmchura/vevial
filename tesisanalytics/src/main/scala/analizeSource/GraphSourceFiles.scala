package analizeSource

import java.io.File

case class GraphSourceFiles(
    nodes: Map[String, SourceFile],
    edges: Map[String, List[String]]
) {
  //Segments or tramos where more than 1 point it, but he does not point anything
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
  val duplexTramo: List[(SourceFile, SourceFile)] = {
    nodes.toList.flatMap {
      case (k, v) =>
        edges
          .getOrElse(k, Nil)
          .filter { e =>
            edges.getOrElse(e, Nil).contains(k)
          }
          .map { o =>
            (v, nodes(o))
          }

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

      }
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

      }
    )
  }

  def copyFilesWithProcess(
      destination: String,
      process: (SourceFile, File) => Unit
  ): Unit = {
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

    largerNodes.foreach { sf =>
      println(sf.inputFile.getName)
      process(sf, directoryResult)

    }
  }

  override def toString: String =
    s"Larger: ${largerNodes.mkString("\n")} \n\nDuplex: ${duplexTramo.mkString("\n")}"
}
