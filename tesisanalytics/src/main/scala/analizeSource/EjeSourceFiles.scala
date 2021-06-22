package analizeSource

case class EjeSourceFiles(
    ejeSourceFiles: List[SourceFile],
    smallerComponents: List[SourceFile]
) {
  val allSourceFiles: List[SourceFile] = ejeSourceFiles ::: smallerComponents
}
