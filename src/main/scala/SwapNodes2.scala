import collections.immutable.BinaryTree
import collections.mutable.{Node, BinaryTree}

import scala.collection.mutable

object SwapNodes2 {
  def main(args: Array[String]): Unit = {
    //    println("asd;lfkj")
    //    for (ln <- io.Source.stdin.getLines) println(ln)
    val treeString =
      """17
        |2 3
        |4 5
        |6 -1
        |-1 7
        |8 9
        |10 11
        |12 13
        |-1 14
        |-1 -1
        |15 -1
        |16 17
        |-1 -1
        |-1 -1
        |-1 -1
        |-1 -1
        |-1 -1
        |-1 -1
        |2
        |2
        |3""".stripMargin
//    val lines = treeString.split("\n")
    val lines = io.Source.stdin.getLines.toArray
    val numNodes = lines(0).toInt
    val nodeLines = lines.slice(1, numNodes + 1)
    val children = nodeLines.map { s =>
      val splits = s.split(" ")
      assert(splits.length == 2)
      (splits(0).toInt, splits(1).toInt)
    }.toList
    val numSwaps = lines(numNodes + 1).toInt
    val swapOps = lines.slice(lines.length - numSwaps, lines.length).map(_.toInt)

    val
    flatNodes = children.flatMap { case (left, right) =>
      List(left, right)
    }.toArray
    val tree = buildTree(children)
    val swappedTrees = swapOps.scanLeft(tree) { case (acc, swapOp) =>
      val t = swapNodes(acc, swapOp)
      println(inOrderTraversal(t).mkString(" "))
      t
    }
  }

  def buildTree(nodes: List[(Int, Int)]): Node[Int] = {
    val queue = new mutable.Queue[Node[Int]]()
    val root = new Node[Int](1)
    queue.enqueue(root)
    nodes.foreach { case (left, right) =>
      val parent = queue.dequeue()
        if (left != -1) {
          parent.left = new Node(left)
          queue.enqueue(parent.left)
        }
        if (right != -1) {
          parent.right = new Node(right)
          queue.enqueue(parent.right)
        }
    }
    root
  }

  def inOrderTraversal(tree: Node[Int]): List[Int] = {
    if (tree == null) Nil
    else if (tree.isLeaf) List(tree.value)
    else {
      inOrderTraversal(tree.left) ::: List(tree.value) ::: inOrderTraversal(tree.right)
    }
  }

  def swap2[T](tree: Node[T]): Node[T] = {
    (tree.left, tree.right) match {
      case (null, null) =>
      case (null, r) =>
        val tmp = r
        tree.right = null
        tree.left = r
      case (l, null) =>
        val tmp = l
        tree.left = null
        tree.right = l
      case (_l, _r) =>
        val tmp = _l
        tree.left = _r
        tree.right = _l
    }
    tree
  }

  def swapNodes[T](tree: Node[T], level: Int): Node[T] = {
    def loop(curLevel: Int, tree: Node[T]): Unit = {
      if (tree == null) Unit
      else if (curLevel % level == 0) {
        swap2(tree)
      }
      if (tree.left != null) loop(curLevel + 1, tree.left)
      if (tree.right != null) loop(curLevel + 1, tree.right)
    }
    loop(1, tree)
    tree
  }

}
