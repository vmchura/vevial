package analizeSource
import io.vmchura.vevial.elementdata.IRIElementData
import io.vmchura.vevial.relevamiento.RelevamientoIRI

import java.io.File
import java.nio.file.StandardCopyOption

case class GraphSourceFilesBuilder private (
    files: List[SourceFile],
    errors: List[String],
    validInterval: Int
) {
  def add(file: File): GraphSourceFilesBuilder = {
    val sf = SourceFile(file)
    if (files.exists(_.equals(sf))) {
      copy(errors =
        s"file ${file.getPath} is already registered, ${files.find(_.equals(sf))}" :: errors
      )

    } else {
      try {
        val relevamiento =
          RelevamientoIRI(sf.inputFile, cd => IRIElementData(cd))
        val interval = relevamiento.interval
        if (Math.abs(interval - validInterval) < 5) {
          try {
            sf.buildEje()
            copy(files = sf :: files)
          } catch {
            case e: Throwable =>
              copy(errors =
                s"file ${file.getPath} can not build eje, ${e.getMessage}" :: errors
              )
          }

        } else {
          copy(errors =
            s"file ${file.getPath} has not a valid interval, validInterval: $validInterval, file's interval: $interval}" :: errors
          )
        }
      } catch {
        case e: Throwable =>
          copy(errors =
            s"file ${file.getPath} is not a valid Relevamiento CSV, error: ${e.getMessage}" :: errors
          )
      }
    }
  }
  def build(): GraphSourceFiles = {

    val nodes = files.map(f => f.hashID -> f).toMap
    val edges = collection.mutable.Map[String, List[String]]()
    def add(from: String, to: String): Unit = {
      val current: List[String] = edges.getOrElse(from, Nil)
      edges += from -> (to :: current)
    }
    files.foreach { larger =>
      val ejeLarger = larger.buildEje()
      println(larger.inputFile.getName)
      files.foreach { shorter =>
        if (!shorter.equals(larger)) {
          for {
            in <- shorter.inOpt
            out <- shorter.outOpt
            _ <- ejeLarger.flatMap(_.projectPointWithDistance(in, 400))
            _ <- ejeLarger.flatMap(_.projectPointWithDistance(out, 400))
          } yield {
            //short is "inside" in Large
            add(shorter.hashID, larger.hashID)
          }
        }

      }
    }

    GraphSourceFiles(
      nodes,
      edges.toList.toMap
    )

  }
}
object GraphSourceFilesBuilder {
  implicit class FileOps(file: File) {
    def isDirectory: Boolean = java.nio.file.Files.isDirectory(file.toPath)
    def isCSV: Boolean = {
      java.nio.file.Files.isRegularFile(file.toPath) && file.getName.endsWith(
        ".csv"
      )
    }
    def isRGD: Boolean = {
      java.nio.file.Files.isRegularFile(file.toPath) && file.getName.endsWith(
        ".rgd"
      )
    }
  }
  def traverse(current: File)(implicit filter: File => Boolean): List[File] = {
    if (current.isDirectory) {
      current.listFiles().flatMap(traverse).toList
    } else {
      if (filter(current)) {
        List(current)
      } else {
        Nil
      }
    }
  }
  def apply(rootDirectory: File): GraphSourceFilesBuilder = {
    require(rootDirectory.isDirectory)
    traverse(rootDirectory)(_.isCSV).foldLeft(
      new GraphSourceFilesBuilder(Nil, Nil, 100)
    ) {
      case (graphBuilder, file) => graphBuilder.add(file)
    }
  }
  def copyRGDFromRootToDirectory(
      rootDirectory: File,
      targetDirectory: File
  ): Unit = {
    require(rootDirectory.isDirectory)
    require(targetDirectory.isDirectory)
    traverse(rootDirectory)(_.isRGD).foreach { f =>
      java.nio.file.Files.copy(
        f.toPath,
        new File(targetDirectory, f.getName).toPath,
        StandardCopyOption.REPLACE_EXISTING
      )
    }
  }

}
