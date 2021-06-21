package analizeSource

import org.scalatest.flatspec.AnyFlatSpec

import java.io.File

class GraphSourceFilesBuilderTest extends AnyFlatSpec {
  behavior of "GraphSourceFilesBuilder"
  it should "search deeper files" in {
    val graphBuilder = GraphSourceFilesBuilder(
      new File("/home/vmchura/Documents/003.Tesis/DataSource/100m")
    )
    assert(graphBuilder.files.length > 200)
  }
  it should "build graph" in {
    val graphBuilder = GraphSourceFilesBuilder(
      new File("/home/vmchura/Documents/003.Tesis/DataSource/100m")
    )
    val graph = graphBuilder.build()
    graph.copyLargeNodesAsKml(
      "/home/vmchura/Documents/003.Tesis/DataSource/PrincipalEjesKML"
    )
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
