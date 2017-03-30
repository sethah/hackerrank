object SmallChallenges {

  /**
   * Pascal rows depend only on the rows before them. We keep only the last row as
   * an array which we can index in constant time, and print as we recurse.
   */
  def printPascal(n: Int): Unit = {
    def loop(acc: Array[Int], level: Int): Array[Int] = {
      if (level > n) acc
      else {
        val nums = Array.tabulate(level) { i =>
          if (i == 0 || i == level - 1) 1
          else acc.apply(i - 1) + acc.apply(i)
        }
        println(nums.mkString(" "))
        loop(nums, level + 1)
      }
    }
    val tmp = loop(Array(), 1)
  }

  def compressString(s: List[Char]): String = {
    // if lastChar = head then recurse,
    def loop(acc: List[Char], last: Char, n: Int, rest: List[Char]): List[Char] = {
      rest match {
        case Nil => //acc
          if (n == 0) acc
          else (n + 1).toString.head :: acc
        case hd :: tl =>
          if (hd == last) loop(acc, last, n + 1, tl)
          else {
            if (n == 0) loop(hd :: acc, hd, 0, tl)
            else loop(hd :: (n + 1).toString.head :: acc, hd, 0, tl)
          }
      }
    }
    if (s.isEmpty) ""
    else loop(List(s.head), s.head, 0, s.tail).reverse.mkString
  }
}
