import collections.mutable.AdjacencyGraph
import scala.collection.mutable

object DFSAlgos extends App {

  val graph = new AdjacencyGraph(6)
  Seq((0, 1), (1, 4), (4, 3), (3, 1), (0, 3), (2, 4), (2, 5), (5, 5)).foreach { case (a, b) =>
    graph.addEdge(a, b, 1.0)
  }
  println(graph)
  val graph2 = new AdjacencyGraph(8)
  Seq((0, 1), (1, 2), (2, 4), (3, 5), (5, 4), (0, 7), (6, 7)).foreach { case (a, b) =>
    graph2.addEdge(a, b, 1.0)
  }

  val parent = scala.collection.mutable.HashMap[Int, Int]()
  /** Recursive with side effects */
  def dfsVisitRec(source: Int): Unit = {
    println(source, parent)
    graph.neighbors(source).foreach { n =>
      if (!parent.contains(n)) {
        parent += (n -> source)
        dfsVisitRec(n)
      }
    }
  }
  dfsVisitRec(0)

  /** Functional version */
  def dfsVisit(source: Int, parent: Map[Int, Int]): Map[Int, Int] = {
    val neighbors = graph.neighbors(source)
    neighbors.foldLeft(parent) { case (p, n) =>
      if (!p.contains(n)) dfsVisit(n, p + (n -> source)) else p
    }
  }
  println(dfsVisit(0, Map.empty[Int, Int]))

  def dfsVisitImp(source: Int, pnt: scala.collection.mutable.Map[Int, Int]) = {
//    val pnt = scala.collection.mutable.Map[Int, Int]()
    val stack = new scala.collection.mutable.Stack[Int]
    stack.push(source)
    while (stack.nonEmpty) {
      val next = stack.pop()
      graph.neighbors(next).foreach { n =>
        if (!pnt.contains(n)) {
          pnt += (n -> next)
          stack.push(n)
        }
      }
    }
    pnt
  }
  println(dfsVisitImp(0, scala.collection.mutable.Map.empty[Int, Int]))

  def dfs(graph: AdjacencyGraph): scala.collection.mutable.Map[Int, Int] = {
    val pnt = scala.collection.mutable.Map[Int, Int]()
    (0 until graph.numNodes).foreach { node =>
      if (!pnt.contains(node)) {
        dfsVisitImp(node, pnt)
      }
    }
    pnt
  }
  println(dfs(graph))

  def topoSort(g: AdjacencyGraph): List[Int] = {
    var componentStacks = List[List[Int]]()
    val pnt = mutable.Map[Int, Int]()
    (0 until g.numNodes).foreach { node =>
      // visit this component of the graph if it hasn't been dfs'd yet
      if (!pnt.contains(node)) {
        // dfs stack is just the typical stack for doing dfs
        // topoList is just the order we visit the nodes, and for each grpah component we want to
        // put the reverse order in which we visited the nodes in that component
        val dfsStack = new mutable.Stack[Int]()
        var topoList = List[Int]()
        dfsStack.push(node)
        while (dfsStack.nonEmpty) {
          val visiting = dfsStack.pop()
          topoList  = visiting :: topoList
          g.neighbors(visiting).foreach { n =>
            if (!pnt.contains(n)) {
              dfsStack.push(n)
              pnt += (n -> visiting)
            }
          }
        }
        componentStacks = topoList.reverse :: componentStacks
      }
    }
    componentStacks.flatten
  }
  println(topoSort(graph2))
}
