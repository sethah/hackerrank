package multithreading

import collections.multithreaded.BlockingQueue

class ThreadPool(numThreads: Int) {

  // need a queue of threads whose run method is just to continuously
  // try to take from the queue
  val threadQueue = new BlockingQueue[Runnable](numThreads)
  val threads = List.fill(numThreads){
    val thread = new MyPoolThread(threadQueue)
    thread.start()
    thread
  }
  @volatile var isStopped = false

  def execute(task: Runnable): Unit = {
    // add this to the blocking queue
    threadQueue.enqueue(task)
  }
  def stop(): Unit = synchronized {
    threads.foreach(_.doStop())
    isStopped = true
  }
}

class MyPoolThread(threadQueue: BlockingQueue[Runnable]) extends Thread {
  private var isStopped: Boolean = false

  override def run(): Unit = {
    while (!isStopped) {
      try {
        val t = threadQueue.dequeue()
        t.run()
      } catch {
        // we catch exceptions because we want to keep the thread pool alive still
        case e: Exception => println(e.getStackTrace())
      }
    }
  }

  def doStop(): Unit = {
    isStopped = true
    // break out of the constant dequeue call
    this.interrupt()
  }

  def stopped: Boolean = isStopped
}
