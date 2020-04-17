package algorithms

import models.TTraversableNode

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DFS[A <: TTraversableNode[A]](visit: A => Unit, unvisitedAdj: A => Seq[A], onCycle: Seq[A] => Unit = (x: Seq[A]) => ()) {

  def run(initial: A): Unit = {
    val alreadyOnStack = mutable.Set.empty[A]
    val stack = mutable.Stack(initial)
    alreadyOnStack += initial

    while(stack.nonEmpty){
      val x = stack.pop()
      if(x.unvisited()){
        visit(x)
        x.markStartTraverse()
      }

      unvisitedAdj(x).foreach{ a =>

        if(alreadyOnStack.contains(a)){
          if(x.parent != a) {
            //cycle found
            val cycle = ListBuffer.empty[A]
            var z = x
            var limitCycle = 10
            while (z != a.parent && limitCycle > 0) {
              limitCycle -= 1
              cycle.append(z)
              z = z.parent

            }
            if (limitCycle <= 0)
              throw new IllegalStateException()
            cycle.append(z)
            cycle.append(a)

            onCycle(cycle.toList)
          }
        }else {
          a.setParent(x)
          stack.push(a)
          alreadyOnStack += a
        }
      }

    }
  }
}
