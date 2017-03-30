import collections.mutable.AdjacencyGraph
import util.FileUtils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object ShortestReach {

  class AdjacencyGraph(n: Int) {
    val graph = Array.fill(n)(Array.fill(n)(AdjacencyGraph.EMPTY_VALUE))

    def checkBounds(idx: Int*): Unit = {
      idx.foreach { i => require(i >= 0, i < n)}
    }

    def addEdge(start: Int, end: Int, weight: Double): Unit = {
      checkBounds(start, end)
      graph(start)(end) = weight
//      graph(end)(start) = weight
    }

    def neighbors(v: Int): Set[Int] = {
      (0 until n).filter { nb =>
        (graph(v)(nb) != AdjacencyGraph.EMPTY_VALUE) && nb != v
      }.toSet
    }

    def BFS(start: Int): Array[Double] = {
      val toVisit = new scala.collection.mutable.Queue[(Int, Int)]
      toVisit.enqueue((0, start))
      val paths = Array.fill(n)(-1.0)
      var iter = 0
      while (toVisit.nonEmpty && iter < 10000) {
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

  def main(args: Array[String]): Unit = {
    val lines = FileUtils.testCase("shortestreach", 7)
    val Array(n, m) = lines.next().trim.split(" ").map(_.toInt)
    val graph = new AdjacencyGraph(n)
    lines.toArray.par.foreach { l =>
      val line = l.trim.split(" ")
      if (line.length == 2) {
        val Array(v1, v2) = line.map(_.toInt)
        // this is an idempotent operation so no synchronization needed
        // as long as nothing else is trying to grab it at the same time
        graph.addEdge(v1 - 1, v2 - 1, 1.0)
      }
    }
//    val futures = (0 until 10).map { i =>
//
//        while (lines.hasNext) {
//          val line = lines.next().trim.split(" ")
//          if (line.length == 2) {
//            val Array(v1, v2) = line.map(_.toInt)
////            if (graph.graph(v1 - 1)(v2 - 1) != AdjacencyGraph.EMPTY_VALUE) c += 1
//            graph.synchronized {
//              graph.addEdge(v1 - 1, v2 - 1, 1.0)
//            }
//          }
//        }
////        println(c)
//    }

//    val x = Future.sequence(futures)
    val numEdges = graph.graph.foldLeft(0) { case (cnt, row) =>
      cnt + row.count(_ != -1.0)
    }
//    println(graph)
    println(numEdges)
  }
}
