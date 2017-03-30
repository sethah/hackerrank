package multithreading

import org.scalatest._

class ThreadPoolSuite extends FunSuite {

  test("parallel sum") {
    val threadPool = new ThreadPool(8)
    val data = (0 until 100).toArray
    val tasks = (0 until 100 by 20).sliding(2).map { pair =>
      new Thread {
        override def run(): Unit = {
          var sum = 0
          (pair(0) until pair(1)).foreach {i => sum += data(i)}
          println(sum)
        }
      }
    }
    tasks.foreach(threadPool.execute)
    tasks.foreach(_.join())
    println(threadPool.isStopped)
    println(threadPool.threads.map(_.stopped).mkString(","))
    threadPool.stop()
    println(threadPool.isStopped)
    println(threadPool.threads.map(_.stopped).mkString(","))
  }
}
