object PlanPower extends App {

  println(planPower(174))

  /**
   * This method uses dynamic programming to tell you what products that you would need
   * to compute in the "power" function to be optimal.
   * @param n
   * @return
   */
  def planPower(n: Int): Set[Int] = {
    val memo = Array.fill(n)(Set.empty[Int])
    def subProblem(k: Int): Set[Int] = {
      if (k == 1) Set(1)
      else if (memo(k - 1).nonEmpty) memo(k - 1)
      else {
        val bestIndex = (1 until k).map { j =>
          (subProblem(j).union(subProblem(k - j)).union(Set(k - j, j)).size, j)
        }.minBy(_._1)._2
        val res = subProblem(bestIndex).union(subProblem(k - bestIndex)).union(Set(k - bestIndex, bestIndex))
        memo(k - 1) = res
        res
      }
    }
    subProblem(n)
  }


}
