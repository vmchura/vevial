package analizeSource

case class GraphSourceFiles(
    nodes: Map[String, SourceFile],
    edges: Map[String, List[String]]
) {
  override def toString: String = s"${edges.values.map(_.length).mkString(",")}"
}
