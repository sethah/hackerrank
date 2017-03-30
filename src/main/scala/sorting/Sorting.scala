package sorting

import scala.annotation.tailrec

object Sorting {

  def main(args: Array[String]): Unit = {
    println(mergeSortNaive(List(3,5,2,1,5,9,2,6,23,5)))
  }

  /**
   * Naive implementation. Not tail recursive.
   */
  def quickSortNaive[T](lst: List[T])(implicit ord: Ordering[T]): List[T] = {
    lst match {
      case Nil => Nil
      case hd :: tl =>
        quickSortNaive(tl.filter(ord.lt(_, hd))) ::: List(hd) :::
          quickSortNaive(tl.filter(ord.gteq(_, hd)))
    }
  }

  /**
   * Tail recursive solution
   * Basically, we build up a stack accumulator of lists to sort, and move them to
   * the sorted list when they are completely sorted.
   */
  def quickSort[T](lst: List[T])(implicit ord: Ordering[T]): List[T] = {
    @tailrec def loop(todo: List[List[T]], done: List[T]): List[T] = {
      todo match {
        case Nil => done
        case next :: rest =>
          next match {
            case Nil => loop(rest, done)
            case hd :: tl =>
              val (left, right) = tl.partition(ord.lt(hd, _))
              // if one of left or right is empty you'll be doing no work
              if (left.isEmpty && right.isEmpty) loop(rest, hd :: done)
              else if (left.isEmpty) loop(right :: rest, hd :: done)
              else if (right.isEmpty) loop(left :: rest, hd :: done)
              else loop(left :: List(hd) :: right :: rest, done)
          }
      }
    }
    loop(List(lst), Nil)
  }

  def mergeSortNaive[T](lst: List[T])(implicit ord: Ordering[T]): List[T] = {
    def merge(left: List[T], right: List[T]): List[T] = {
      (left, right) match {
        case (Nil, _) => right
        case (_, Nil) => left
        case (x :: xs, y :: ys) =>
          if (ord.gt(x, y)) y :: merge(ys, left) else x :: merge(xs, right)
      }
    }
    def sort(values: List[T]): List[T] = {
      values match {
        case Nil => Nil
        case x :: Nil => values
        case x :: xs =>
          val (left, right) = values.splitAt(values.length / 2)
          merge(sort(left), sort(right))
      }
    }
    sort(lst)
  }
}
