package collections.mutable

import scala.collection.mutable

trait Graph[T] {

  def numNodes: Int

  def nodeIndex(node: T): Int

  def neighbors(nodeIndex: Int): Set[Int]

}


class AdjacencyGraph(val numNodes: Int) extends Graph[Int] {
  val graph = Array.fill(numNodes)(Array.fill(numNodes)(AdjacencyGraph.EMPTY_VALUE))

  def nodeIndex(node: Int): Int = node - 1

  def checkBounds(idx: Int*): Unit = {
    idx.foreach { i => require(i >= 0, i < numNodes)}
  }

  def addEdge(start: Int, end: Int, weight: Double): Unit = {
    checkBounds(start, end)
    graph(start)(end) = weight
  }

  def neighbors(v: Int): Set[Int] = {
    (0 until numNodes).filter { nb =>
      (graph(v)(nb) != AdjacencyGraph.EMPTY_VALUE) && nb != v
    }.toSet
  }

  def BFS(start: Int): Array[Double] = {
    val maxIter = 1000
    val toVisit = new scala.collection.mutable.Queue[(Int, Int)]
    toVisit.enqueue((0, start))
    val paths = Array.fill(numNodes)(-1.0)
    var iter = 0
    while (toVisit.nonEmpty && iter < maxIter) {
      val (prevLength, visiting) = toVisit.dequeue()

      neighbors(visiting).foreach { n =>
        if (paths(n) == -1.0 && n != start) {
          paths(n) = prevLength + 1
          toVisit.enqueue((prevLength + 1, n))
        }
      }
      iter += 1
    }
    paths
  }

  override def toString: String = {
    val sb = new StringBuilder()
    graph.foreach { row => sb.append(row.mkString(",") + "\n")}
    sb.result()
  }
}

object AdjacencyGraph {

  val EMPTY_VALUE = -1.0

}

