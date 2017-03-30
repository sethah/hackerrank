import collections.mutable.AdjacencyGraph
import scala.collection.mutable

object BFSAlgos extends App {

  val graph = new AdjacencyGraph(6)
  Seq((0, 1), (1, 4), (4, 3), (3, 1), (0, 3), (2, 4), (2, 5), (5, 5)).foreach { case (a, b) =>
    graph.addEdge(a, b, 1.0)
  }

  def singleSourceShortestPath(graph: AdjacencyGraph, source: Int): mutable.Map[Int, Int] = {
    val toVisit = new scala.collection.mutable.Queue[Int]()
    toVisit.enqueue(source)
    val parent = scala.collection.mutable.Map[Int, Int]()
    while (toVisit.nonEmpty) {
      val visiting = toVisit.dequeue()
      graph.neighbors(visiting).filterNot(parent.contains).foreach { x =>
        toVisit.enqueue(x)
        parent += (x -> visiting)
      }
    }
    parent
  }

  println(singleSourceShortestPath(graph, 0))

}
