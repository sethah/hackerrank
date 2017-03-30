//import ShortestReach.AdjacencyGraph
import collections.mutable._

import scala.collection.mutable._

object Djikstra {
  def main(args: Array[String]): Unit = {
    val graph = new AdjacencyGraph(9)
    val edges = Seq(
      (0, 1, 3.0),
      (0, 3, 1.0),
      (0, 4, 2.0),
      (1, 5, 4.0),
      (2, 0, 0.0),
      (3, 7, 1.0),
      (4, 2, 7.0),
      (4, 8, 2.0),
      (5, 2, 1.0),
      (6, 0, 3.0),
      (7, 8, 1.0),
      (7, 6, 5.0),
      (8, 5, 3.0)
    )
    edges.foreach { case (start, end, weight) => graph.addEdge(start, end, weight)}
    println(shortestPath(graph, 0))

  }

  /**
   * A inefficient but simple implementation of Djikstra shortest path algo. Duplicates values on
   * the priority queue instead of actually modifying the distances in the relax phase. This way,
   * we don't need a decrease key operation.
   */
  def shortestPath(graph: AdjacencyGraph, start: Int): Map[Int, Double] = {
    val finished = HashMap[Int, Double]()
    val pq = PriorityQueue.empty[(Double, Int)](
      implicitly[Ordering[(Double, Int)]].reverse
    )
    (0 until graph.numNodes).foreach { n => pq.enqueue(Double.MaxValue -> n)}
    val pnt = new scala.collection.mutable.HashMap[Int, (Int, Double)]()
    pq.enqueue(0.0 -> start)
    var iter = 0
    while (pq.nonEmpty && finished.size < graph.numNodes && iter < 40) {
      var visiting = -1
      var dist = -1.0
      while (finished.contains(visiting) || visiting == -1) {
        // In this implementation we don't actually change the distance values in the relax step,
        // we just enqueue a new tuple. We'll still dequeue it properly but the old unrelaxed values
        // for a node are still going to be on there after that node is finished. So here, we pop
        // off nodes until we get one that isn't finished already
        val tmp = pq.dequeue()
        visiting = tmp._2
        dist = tmp._1
      }
      finished += (visiting -> dist)
      val neighbors = graph.neighbors(visiting)
      neighbors.foreach { n =>
        // just enqueue the relaxed values
        val relaxed = dist + graph.graph(visiting)(n)
        pq.enqueue((relaxed, n))
        println(n, visiting)
        if (!pnt.contains(n) || pnt(n)._2 > relaxed) {
          pnt += (n -> (visiting, relaxed)) // not correct, have to keep track of distance it took to get there
        }
      }
      iter += 1
    }
    println(pnt)
    finished
  }
}
