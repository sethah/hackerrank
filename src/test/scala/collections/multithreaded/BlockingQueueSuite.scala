package collections.multithreaded

import org.scalatest._

class BlockingQueueSuite extends FunSuite {

  test("resize") {
    val q = new BlockingQueue[Int](1000)
    Seq(0, 1, 2).foreach(q.enqueue)
    println(q)
    q.enqueue(3)
    println(q.iterator.toArray.mkString(","))
    q.enqueue(4)
    println(q)
    println(q.shouldIncreaseCapacity)
    q.dequeue()
    println(q.shouldDecreaseCapacity)
    q.dequeue()
    println(q.shouldDecreaseCapacity)
    q.dequeue()
    println(q.shouldDecreaseCapacity)
    println(q)
  }

  test("simple test") {
    val q = new BlockingQueue[Int](1000)
    val n = 1000
    val values = (0 until n).toArray
    values.foreach { q.enqueue}
    val res = values.map(x => q.dequeue())
    assert(res === values)
  }

  test("multithreading") {
    val n = 100
    val q = new BlockingQueue[Int](1000)
    (0 until 10).foreach(q.enqueue)
    val rng = new scala.util.Random(42)
    (0 until n).par.foreach { q.enqueue }
    (0 until n).par.foreach { x => q.dequeue() }
    println(q)
//    (0 until n).par.foreach { i =>
//      if (rng.nextDouble() > 0.8) q.enqueue(i) else q.dequeue()
//    }
  }
}
