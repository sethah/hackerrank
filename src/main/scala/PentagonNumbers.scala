
object PentagonNumbers {
  val memo = new scala.collection.mutable.HashMap[Long, Long]()
  memo += (1L -> 1L)
  def main(args: Array[String]): Unit = {
//    val lines = io.Source.stdin.getLines()
    val lines = Seq(4, 1, 2, 3, 4, 5).iterator
    lines.next()
    while (lines.hasNext) {
      val n = lines.next().toInt
      println(pentagonalNums(n))
//      println(memo)
    }
  }

  def pentagonalNums(n: Long): Long = {
    // if memo contains n then return the memoized value
    // otherwise pentagonalNums(n - 1) + 3 * (n - 1) + 2
    if (memo.contains(n)) memo(n)
    else {
      pentagonalNums(n - 1)
      memo += (n -> (memo(n - 1) + 3 * (n - 1) + 1))
      memo(n)
    }
  }
  def pentagonalNums2(n: Long): Long = {
    def loop(acc: Long, cur: Long): Long = {
      if (memo.contains(cur)) memo(cur) + acc
      else if (cur == 1) {
        val res = acc + 1
        memo += (n -> res)
        res
      }
      else {
        val res = loop(acc + 3 * (cur - 1) + 1, cur - 1)
        memo += (n -> res)
        res
      }
    }
    loop(0, n)
    // if memo contains n then return the memoized value
    // otherwise pentagonalNums(n - 1) + 3 * (n - 1) + 2
  }
}
