package analizeSource

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
  override def toString: String =
    s"Larger: ${largerNodes.mkString("\n")} \n\nDuplex: ${duplexTramo.mkString("\n")}"
}
