package algorithms

import models.{Edge, Graph, TNode, TreeGraph}

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
        val dfsAlgorithm = new DFS[A]((x: A) => nodesThisSet.append(x),
          a => graphMap.getOrElse(a,ListBuffer.empty[A]).toList,
          cycle => cycle.zip(cycle.tail).foreach{ case (x,y) =>
            dsu.union_sets(x,y)
          })

        dfsAlgorithm.run(a)
        val treeNodes: List[A] = nodesThisSet.toList



        val edges = treeNodes.flatMap { n =>
          if(graphMap.contains(n)) {
            val adj = graphMap(n)
            adj.map { v =>
              Edge(dsu.find_set(n), dsu.find_set(v))
            }
          }else{
            Nil
          }

        }.distinct.toSet


        val g = TreeGraph(treeNodes.map(dsu.find_set).distinct, edges)
        treesFound.append(g)


      }
    }

    treesFound.toList
  }
}
