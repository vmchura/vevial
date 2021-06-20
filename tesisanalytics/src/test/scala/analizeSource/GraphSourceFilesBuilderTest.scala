package analizeSource

import org.scalatest.flatspec.AnyFlatSpec

import java.io.File

class GraphSourceFilesBuilderTest extends AnyFlatSpec {
  behavior of "GraphSourceFilesBuilder"
  it should "search deeper files" in {
    val graphBuilder = GraphSourceFilesBuilder(
      new File("/home/vmchura/Documents/003.Tesis/DataSource/20m")
    )
    assertResult(249)(graphBuilder.files.length)
    assert(graphBuilder.errors.isEmpty)
  }
  it should "traverse and copy all .rgd" in {
    val rootDirectory = new File("/home/vmchura/Documents/003.Tesis/DataSource")
    val allDirectory = new File(rootDirectory, "All")
    val targetDirectory = new File(rootDirectory, "20m")

    GraphSourceFilesBuilder.copyRGDFromRootToDirectory(
      allDirectory,
      targetDirectory
    )
    assert(targetDirectory.listFiles().length > 0)
  }
}