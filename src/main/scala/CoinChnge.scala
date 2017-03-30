object CoinChange extends App {

  def coinChange(n: Int, coins: Set[Int]): Int = {
    def subProblem(k: Int, _coins: Set[Int]): Int = {
      if (k < 0) 0
      else if (k == 0) 1
      else if (_coins.isEmpty) 0
      else {
        val c = _coins.head
        val tmp1 = subProblem(k - c, _coins)
        val tmp2 = subProblem(k, _coins - c)
        tmp1 + tmp2
      }
    }
    subProblem(n, coins)
  }

  def canMakeChange(n: Int, coins: Set[Int]): Boolean = {
    val memo = Array.fill(n + 1)(false)
    memo(0) = true
    (0 to n).foreach { k =>
      if (k < 0) false
      else if (k == 0) true
      else {
        val res = coins.filter(_ <= k).exists(c => memo(k - c))
        memo(k) = res
        res
      }
    }
//    println(memo.mkString(","))
    memo(n)
//    def subProblem(k: Int): Boolean = {
//      if (k < 0) false
//      else if (k == 0) true
//      else {
//        coins.exists(c => subProblem(k - c))
//      }
//    }
//    subProblem(n)
  }

  val coins = Set(2, 5, 3, 7)
  println(canMakeChange(6, coins))
  println(coinChange(10, coins))
}
