package analizeSource

object StronglyConnectedComponents {
  def buildSCC[K, V](
      nodes: Map[K, V],
      edges: Map[K, List[K]]
  ): List[List[V]] = {
    val edgesIn: Map[K, List[K]] = {
      nodes.keys.map { key =>
        //find all edges which points to key
        val in = edges.flatMap {
          case (k, edgesOut) =>
            Option.when(edgesOut.contains(key))(k)
        }
        key -> in.toList
      }.toMap
    }

    val indx: Map[K, Int] = nodes.keys.zipWithIndex.map {
      case (k, i) => k -> i
    }.toMap
    val n = nodes.size
    val component = Array.fill(n)(-1)
    val visited = Array.fill(n)(false)
    val L = scala.collection.mutable.Stack[K]()

    def visit(sfID: K): Unit = {
      val i = indx(sfID)
      if (!visited(i)) {
        visited(i) = true
        edges.getOrElse(sfID, Nil).foreach { eOut =>
          visit(eOut)
        }
        println(s"Stacking $sfID")
        L.push(sfID)
      }
    }
    def assign(sfID: K, root: K): Unit = {
      val i = indx(sfID)
      if (component(i) == -1) { // no component
        if (sfID == root) {
          component(i) = i
        } else {
          component(i) = component(indx(root))
        }
        println(s"component($i) <= ${component(i)}")
        edgesIn.getOrElse(sfID, Nil).foreach { eIn =>
          assign(eIn, root)
        }
      }
    }
    val bag = Array.fill(n)(List.empty[V])

    nodes.keys.foreach(visit)
    while (L.nonEmpty) {
      val v = L.pop()
      assign(v, v)
    }
    require(!component.contains(-1), "All elements are assigned")
    nodes.foreach {
      case (k, sf) =>
        bag(component(indx(k))) = sf :: bag(component(indx(k)))
    }
    bag.filter(_.nonEmpty).toList
  }
}
