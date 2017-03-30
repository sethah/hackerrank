import collections.mutable.{AdjacencyGraph, Graph}
import java.util.concurrent._

import util.FileUtils

import scala.collection.mutable._

object GraphAlgos {

  def main(args: Array[String]): Unit = {
    val lines = FileUtils.testCase("cellsinagrid", 6)
//    val lines = io.Source.stdin.getLines()
    val m = lines.next().toInt
    val n = lines.next().toInt
    println(m, n)
    val graphArray = Array.fill(m, n)(0)
    var rowIdx = 0
    while (lines.hasNext) {
      val line = lines.next()
      line.trim.split(" ").zipWithIndex.foreach { case (value, colIdx) =>
        graphArray(rowIdx)(colIdx) = value.toInt
      }
      rowIdx += 1
    }

//    graphArray.foreach(r => println(r.mkString(",")))

    def neighbors(r: Int, c: Int): scala.collection.immutable.IndexedSeq[(Int, Int)] = {
      for (i <- (r - 1 to r + 1);
           j <- (c-1  to c+1);
           if i >= 0 && i < m && j < n && j >= 0 && graphArray(i)(j) == 1 && !(i == r && j == c))
      yield (i, j)
    }
//    println(neighbors(3, 2))
    var islands = List[HashSet[(Int, Int)]]()
    (0 until m).foreach { row =>
      var col = 0
      while (col < n) {
        if (graphArray(row)(col) == 1 && islands.forall { island => !island.contains((row, col))}) {
          var nodeStack = List[(Int, Int)]((row, col))
          val visited = HashSet[(Int, Int)]()
          while (nodeStack.nonEmpty) {
            val visiting = nodeStack.head
            nodeStack = nodeStack.tail
            visited += visiting
            // visit all neighbors that haven't been visited
            neighbors(visiting._1, visiting._2).filterNot(visited.contains).foreach { n =>
              nodeStack = n :: nodeStack
            }
          }
          islands = visited :: islands
        }
        col += 1
      }
    }
//    islands.foreach(println)
    println(islands.map(island => island.size).max)
  }

  def ShortestReachBFS[T](graph: Graph[T], start: T): Array[Double] = {
    // (currentLength, nodeIndex)
    val toVisit = new ConcurrentLinkedQueue[(Int, Int)]()
    toVisit.add((0, graph.nodeIndex(start)))
    val paths = Array.fill(graph.numNodes)(-1.0)
    val maxIter = 10000
    var iter = 0
    while (!toVisit.isEmpty && iter < maxIter) {
      val (prevLength, visiting) = toVisit.poll()
      graph.neighbors(visiting).foreach { n =>
        if (paths(n) == -1.0 && n != start) {
          paths(n) = prevLength + 1
          toVisit.add((prevLength + 1, n))
        }
      }
      iter += 1
    }
    paths
  }

}
