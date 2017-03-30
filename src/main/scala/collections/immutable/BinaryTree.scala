package collections.immutable

trait BinaryTree[+T] {

  def left: BinaryTree[T]
  def right: BinaryTree[T]
  def data: T
  def depth: Int
  def isLeaf: Boolean = depth == 1

}

object BinaryTree {
  def inOrderTraversal[T](tree: BinaryTree[T]): List[T] = {
    def loop(acc: List[T], _tree: BinaryTree[T]): List[T] = {
      _tree match {
        case NilTreeNode => acc
        case ConsTreeNode(d, l, r) =>
          val flatLeft = loop(d :: acc, l)
          val flatRight = loop(flatLeft, r)
          flatRight
      }
    }
    loop(Nil, tree).reverse
  }

  def swapNodes[T](tree: BinaryTree[T], nodeIndex: Int): BinaryTree[T] = {
    def loop(curIdx: Int, tree: BinaryTree[T]): BinaryTree[T] = {
      tree match {
        case NilTreeNode => tree
        case ConsTreeNode(d: T, l, r) =>
          if (curIdx == nodeIndex) {
            val newLeft = new ConsTreeNode[T](r.data, l.left, l.right)
            val newRight = new ConsTreeNode[T](l.data, r.left, r.right)
            new ConsTreeNode[T](d, newLeft, newRight)
          }
          else new ConsTreeNode(d, loop(curIdx * 2, l), loop(curIdx * 2 + 1, r))
      }
    }
    loop(1, tree)
  }
}

case class ConsTreeNode[T](data: T, left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T] {
  def depth: Int = 1 + (left.depth max right.depth)
}

case object NilTreeNode extends BinaryTree[Nothing] {
  def left: BinaryTree[Nothing] = throw new NotImplementedError()
  def right: BinaryTree[Nothing] = throw new NotImplementedError()
  def data: Nothing = throw new NotImplementedError()
  def depth: Int = 0
}
