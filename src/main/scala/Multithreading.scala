object Multithreading {

  class VolatileDoubleArray(val length : Int){
    val array = new Array[Double](length);
    @volatile var marker = 0;
    def apply(i : Int) = {marker; array(i); }
    def update(i : Int, x : Double) { array(i) = x; marker = 0; }
    def inc(i: Int, x: Int) = synchronized {
      val tmp = array(i)
      array(i) = tmp + 1
    }
  }

  @volatile var marker = 0
  val arr = new Array[Int](10)
//  val arr = new VolatileDoubleArray(10)

  def main(args: Array[String]): Unit = {
    (0 until 500).par.foreach { i =>
      scala.util.Random.shuffle((0 until arr.length).toList).foreach { j =>
//        arr.inc(j, 1)
        arr.synchronized {
          val tmp = arr(j)
          arr(j) = tmp + 1
        }
      }
    }
    println(arr.mkString(","))
  }

}
