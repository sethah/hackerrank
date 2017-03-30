object StepsToN extends App {
  val _n = 4
  val memo = Array.fill(_n + 1)(-1)
  def stepsToN(n: Int): Int = {
    println(n)

    // TODO: just store the last three
    if (n == 0) 1
    else if (n == 1) 1
    else if (n == 2) 2
    else {
      // stepsToN
      val validSteps = (1 to 3).filter(n - _ >= 0)
      val res = validSteps.map(x => stepsToN(n - x)).sum
      memo(n) = res
      res
    }

  }

  println(stepsToN(_n))
  println(memo.mkString(","))

}
