package algorithms

import models.{Graph, TNode, TreeGraph}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object BasicTreatment {

  /**
    * Given a Undirected graph, returns collection of trees
    * @param graph
    * @tparam A
    * @return
    */
  def buildTree[A <: TNode[A]](graph: Graph[A]): Seq[TreeGraph[A]] = {
    val graphMapLB: mutable.Map[A,ListBuffer[A]] = mutable.Map.empty[A,ListBuffer[A]]
    val dsu = new DSU(graph.nodes)
    def appendToGraph(x: A, y: A): Unit = if(graphMapLB.contains(x)) graphMapLB(x).append(y) else graphMapLB += x -> ListBuffer(y)
    graph.edges.foreach{ e =>
      appendToGraph(e.from,e.to)
      appendToGraph(e.to,e.from)
    }
    val graphMap: Map[A,Set[A]] = graphMapLB.map{case (a,b) => a -> b.toSet}.toMap

    def dfs(a: A)(implicit atStart: A => Unit): Unit = {
      a.markStartTraverse()
      atStart(a)

      if(graphMap.contains(a)){
        val adj = graphMap(a)
        adj.foreach{ v =>
          if(v.unvisited()){
            v.setParent(a)
            dfs(v)
          }else{
            if(v.finished()){
              ()
            }else{
              if(a.parent != v){
                val cycle = ListBuffer(v)
                var x = a
                while(x.parent != v){
                  cycle.append(x)
                  x = x.parent
                }
                cycle.append(x)

                cycle.zip(cycle.tail).foreach{ case (x,y) =>
                  dsu.union_sets(x,y)
                }


              }
            }
          }



        }
      }
      a.markFinishTraverse()
    }

    graph.nodes.foreach(_.resetState())

    val treesFound = ListBuffer.empty[TreeGraph[A]]
    graph.nodes.foreach{ a =>
      if(a.unvisited()){
        val nodesThisSet = ListBuffer.empty[A]
        implicit val atStart: A => Unit =  (x: A) => nodesThisSet.append(x)
        dfs(a)
        val treeNodes: List[A] = nodesThisSet.toList
        val edges = treeNodes.zip(treeNodes.tail).flatMap{
          case (x,y) => {
            if((graphMap.contains(x) && graphMap(x).contains(y)) ||
              (graphMap.contains(y) && graphMap(y).contains(x))){
              val a = dsu.find_set(x)
              val b = dsu.find_set(y)
              Seq((a,b),(b,a))
            }else{
              Nil
            }


          }
        }

        val g = TreeGraph(treeNodes.map(dsu.find_set).distinct,(x : A,y: A) => edges.contains((x,y)))
        treesFound.append(g)

      }
    }

    treesFound.toList
  }
}
