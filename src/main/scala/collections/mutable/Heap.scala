package collections.mutable

import scala.reflect.ClassTag

object Heap {
  def main(args: Array[String]): Unit = {
    val hp = new BinaryHeap[Int]
    val rng = new scala.util.Random(42)
    val data = Array.fill(1000)(rng.nextInt % 10)
    val srtd = data.sorted
    data.foreach { i =>
      hp.enqueue(i)
    }
    (0 until data.length).foreach { i =>
      val tmp = hp.dequeue()
      assert(srtd(i) == tmp)
    }
  }
}

trait PriorityQueue[T] {
  def enqueue(o: T)

  def dequeue(): T

  def isEmpty: Boolean

  def peek: T
}

class BinaryHeap[T: ClassTag](implicit ord: Ordering[T]) extends PriorityQueue[T] {

  import ord._
  // we will use an array to store our tree. In a min-heap every parent is less than both
  // its children. Since we know the tree is always complete, we can easily index it using an array

  private val INITIAL_LENGTH = 10
  private val MIN_SIZE = 10
  private var data = new Array[T](INITIAL_LENGTH)
  private var back = 1
  private val front = 1
  private var capacity = INITIAL_LENGTH

  def length: Int = back - front

  def peek: T = data(back - 1)

  /**
   * Move the item at i up until it meets the heap condition.
   * @param i
   */
  private def bubbleUp(i: Int): Unit = {
    var idx = i
    val tmp = data(idx)
    while (tmp < data(idx / 2) && idx > 1) {
      data(idx) = data(idx / 2)
      idx = idx >> 1
    }
    data(idx) = tmp
  }

  def enqueue(item: T): Unit = {
    // put it at the back, then move it upwards until it fits
    maybeResize()
    data(back) = item
    bubbleUp(back)
    back += 1
  }

  def dequeue(): T = {
    if (isEmpty) throw new NoSuchElementException("empty queue")
    val popped = data(1)
    data(1) = data(back - 1)
    back -= 1
    var idx = 1
    var stop = false
    while (!stop && idx * 2 < back) {
      val (smallerChild, largerChild) = if (data(idx * 2) < data(idx * 2 + 1)) {
        (idx * 2, idx * 2 + 1)
      } else {
        (idx * 2 + 1, idx * 2)
      }
      if (data(idx) > data(smallerChild)) {
        val tmp = data(idx)
        data(idx) = data(smallerChild)
        data(smallerChild) = tmp
        idx = smallerChild
      } else if (data(idx) > data(largerChild)) {
        val tmp = data(idx)
        data(idx) = data(largerChild)
        data(largerChild) = tmp
        idx = largerChild
      } else {
        stop = true
      }
    }
    maybeResize()
    popped
  }

  def compareAndSwap(i: Int, j: Int): Boolean = {
    val res = data(i) < data(j)
    if (res) {
      val tmp = data(i)
      data(i) = data(j)
      data(j) = tmp
    }
    res
  }

  def isEmpty: Boolean = back == 1

  def maybeResize(): Unit = {
    if (shouldIncreaseCapacity) doResize(capacity * 2)
    else if (shouldDecreaseCapacity) doResize(capacity / 2)
  }

  def doResize(newCapacity: Int): Unit = {
    val newData = new Array[T](newCapacity)
    System.arraycopy(data, front, newData, 1, length + 1)
    data = newData
    back = length + 1
    capacity = newCapacity
  }

  def shouldIncreaseCapacity: Boolean = {
    val fractionUsed = length.toDouble / capacity
    fractionUsed > 0.75
  }

  def shouldDecreaseCapacity: Boolean = {
    val fractionUsed = length.toDouble / capacity
    fractionUsed < 0.25 && capacity / 2 >= MIN_SIZE
  }

  override def toString: String = {
    data.zipWithIndex.foldLeft("") { case (s, (x, i)) => if (i == back - 1) s + x + "|,"
    else s + x + ","}
  }
}
