object ItemAtK extends App {

  val rng = new scala.util.Random(32)
  val data = List.fill(200)(rng.nextInt % 100)
  val srtd = data.sorted
  println(srtd)
  println(itemAtK(data, 43), srtd(43 - 1))
  println(itemAtK(data, 112), srtd(112 - 1))
  println(itemAtK(data, 82), srtd(82 - 1))

  def itemAtK(arr: List[Int], k: Int): List[Int] = {
    // define a recursive function which
    // choose a random pivot, then partition the list on either side of it
    // if there are > k elements to the left of it then call itemAtK on that
    // if there are < k elements to the left then call itemAtK(right, k - left.length)
    arr match {
      case Nil => Nil
      case hd :: Nil => List(hd)
      case hd :: tl =>
        val (left, right) = tl.partition(_ < hd)
//        println(left.sorted, right.sorted, k, left.length, right.length)
        if (left.length == k - 1) List(hd)
        else if (left.length >= k) itemAtK(left, k)
        else itemAtK(right, k - left.length - 1)
    }
  }

}
