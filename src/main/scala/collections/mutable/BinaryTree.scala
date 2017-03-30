package collections.mutable

trait BinaryTree[T] {

  def value: T
  def parent: Node[T]
  def left: Node[T]
  def right: Node[T]
  def isLeaf: Boolean
  def isRoot: Boolean
  def depth: Int

}

class Node[T](
    var value: T,
    var parent: Node[T],
    var left: Node[T],
    var right: Node[T]) extends BinaryTree[T] {

  def this(value: T) = this(value, null, null, null)

  def this(value: T, parent: Node[T]) = this(value, parent, null, null)

  def depth = {
    if (isLeaf) 1
    else if (left == null) 1 + right.depth
    else if (right == null) 1 + left.depth
    else 1 + (right.depth max left.depth)
  }

  def isLeaf = (left == null) && (right == null)

  def isRoot = parent == null

}
