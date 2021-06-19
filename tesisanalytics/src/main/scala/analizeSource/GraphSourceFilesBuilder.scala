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
        val interval =
          RelevamientoIRI(sf.inputFile, cd => IRIElementData(cd)).interval
        if (Math.abs(interval - validInterval) < 5) {
          copy(files = sf :: files)
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
      new GraphSourceFilesBuilder(Nil, Nil, 20)
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
