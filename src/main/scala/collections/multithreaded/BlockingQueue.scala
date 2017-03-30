package collections.multithreaded

import scala.reflect.ClassTag

class BlockingQueue[T: ClassTag](maxSize: Int) {

  private val initialSize = 10
  private var queue = new Array[T](initialSize)
  private var capacity = initialSize
  var front = 0
  var rear = 0
  private var numElems = 0
  private val MIN_SIZE = 5

  val lock = new Object()

  // NOTE: synchronizing on enqueue and dequeue isn't good enough since two threads can still
  // get into enqueue and dequeue. Two threads cannot get into enqueue at the same time, but
  // we still have problems. We need get a lock on queue I think
  // Update: NOPE! enqueue and dequeue have to be able to happen simultaneously! Otherwise,
  // they just wait for each other.

  def enqueue(item: T): Unit = synchronized {
    while (numElems == capacity) wait()
    // notifiy dequeue threads that we put something in
    if (numElems == 0) notifyAll()
    maybeResize()
    require(front >= 0 && front < capacity, s"front: $front, capacity: $capacity")
    lock.synchronized {
      queue(front) = item
      front = if (front == capacity - 1) 0 else front + 1
      numElems += 1
    }
  }

  def dequeue(): T = synchronized {
    while (numElems == 0) wait()
    // we do this because enqueue threads may have been waiting to put another item in
    // now we take one out so there is room
    if (numElems == capacity) notifyAll()
    maybeResize()
    require(rear >= 0 && rear < capacity)
    var res: T = new Array[T](1).head
    lock.synchronized {
      res = queue(rear)
      rear = if (rear == capacity - 1) 0 else rear + 1
      numElems -= 1
    }
    res
  }

  def shouldIncreaseCapacity: Boolean = {
    val fractionUsed = numElems.toDouble / capacity
    fractionUsed > 0.75 && capacity * 2 < maxSize
  }

  def shouldDecreaseCapacity: Boolean = {
    val fractionUsed = numElems.toDouble / capacity
    fractionUsed < 0.25 && capacity / 2 >= MIN_SIZE
  }

  def maybeResize(): Unit = lock.synchronized {
    if (shouldIncreaseCapacity) {
      // double capacity
      println(s"increasing size from $capacity to ${2 * capacity}")
      val newQ = new Array[T](capacity * 2)
      iterator.zipWithIndex.foreach { case (v, i) => newQ(i) = v}
      queue = newQ
      front = numElems
      rear = 0
      capacity = newQ.length
    } else if (shouldDecreaseCapacity) {
      // halve capacity
      println(s"decreasing size from $capacity to ${capacity / 2}")
      val newQ = new Array[T](capacity / 2)
      iterator.zipWithIndex.foreach { case (v, i) => newQ(i) = v}
      front = numElems
      rear = 0
      queue = newQ
      capacity = newQ.length
    }
  }

  def iterator: Iterator[T] = new Iterator[T] {
    var tmpCount = numElems
    var rover = rear
    val _capacity = queue.length
    override def hasNext: Boolean = {
      tmpCount > 0
    }

    override def next(): T = {
      if (!hasNext) throw new NoSuchElementException("empty iterator")
      val ret = queue(rover)
      if (rover == _capacity - 1) rover = 0 else rover += 1
      tmpCount -= 1
      ret
    }
  }

  override def toString: String = {
    val vstring = queue.zipWithIndex.map { case (x, i) =>
      if (i == front && i == rear) x + "rf"
      else if (i == front) x + "f"
      else if (i == rear) x + "r"
      else x
    }.mkString(",")
    s"($numElems, $capacity, $front, $rear): $vstring"
  }
}
