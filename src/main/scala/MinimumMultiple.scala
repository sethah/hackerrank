import scala.annotation.tailrec

object MinimumMultiple {

  def main(args: Array[String]): Unit = {
    val treeString =
      """5
        |2 5 6 1 9
        |7
        |Q 0 4
        |U 1 2
        |Q 0 2
        |Q 3 4
        |Q 2 4
        |U 3 8
        |Q 2 3""".stripMargin
    val ns = Array(1300,3,12,1736,29029,120,560,322,748,207,407,2,19,1,1116,560224,18,744,76,162,450,54,238,7,61200,7,14,9,6,2486400,12,413,89640,19,6,2,14)
    println("lcm", lcm(ns.map(_.toLong).toList) % (1e9.toLong + 7))
//    val lines = treeString.split("\n").iterator
    val lines = scala.io.Source.fromFile("/Users/sethhendrickson/Development/hackerrank/minmultcase2.txt").getLines()
//    val lines = io.Source.stdin.getLines()
    lines.next()
    val nums = lines.next().split(" ").map(_.toLong)
    println(nums.length)
    lines.next()
    while (lines.hasNext) {
      val Array(a, b, c) = lines.next().split(" ")
      if (a == "U") {
        val tmp = nums(b.toInt)
        nums(b.toInt) *= c.toLong
        println(tmp, nums(b.toInt), "a;lsdfkja")
      } else {
//        val rawLCM = lcm(nums.slice(b.toInt, c.toInt + 1).toList)
        val rawLCM = lcm(nums, b.toInt, c.toInt)
        println(nums.mkString(","))
        println(rawLCM % (1e9.toLong + 7))
      }
    }
  }


  def myFib(n: Int, stream: Stream[Int]): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else {
      stream(n - 1) + stream(n - 2)
    }
  }

  val rawStream = Stream.continually(0)
  val fib = (n: Int) => {
    if (n == 0) 0
    else if (n == 1) 1
    else memoFib(n - 1) + memoFib(n - 2)
  }
  val memoFib: (Int) => Int = rawStream.map(fib).apply(_)

  @tailrec def itemAtK[T](k: Int, lst: List[T])(implicit ord: Ordering[T]): T = {
    require(k < lst.length)
    lst match {
      case Nil => throw new IllegalArgumentException("median of empty list")
      case hd :: tl =>
        val (right, left) = tl.partition(ord.lt(hd, _))
        val leftSize = left.length
        if (leftSize == k) hd
        else if (leftSize > k) itemAtK(k, left)
        else itemAtK(k - leftSize - 1, right)

    }
  }

  def median[T](lst: List[T])(implicit ord: Ordering[T]): T = {
    itemAtK(lst.length / 2 - 1, lst)
  }

  def gcd(a: Long, b: Long): Long = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  def lcm(a: Long, b: Long): Long = {
    if ((a * b / 10000) != ((a / 10000) * b)) println("asldkfja;ldfj", a, b)
    a * (b / gcd(a, b))
  }

  def lcm(nums: List[Long]): Long = {
    nums.tail.foldLeft(nums.head) { case (x, y) =>
      lcm(x, y)
    }
  }

  def lcm(nums: Array[Long], start: Int, end: Int): Long = {
    var i = start + 1
    var _lcm = nums(start)
    while (i < end + 1) {
      _lcm = lcm(_lcm, nums(i))
      i += 1
    }
    _lcm
  }

}
