object PrintFactors extends App {

  def print(n: Int): List[List[Int]] = {
    def subProblem(k: Int, target: Int): List[List[Int]] = {
      println(k, target)
      if (k == 1) List(List(1))
      else {
        (k until 1 by -1).filter(j => target % j == 0).flatMap { j =>
          subProblem(target / j, j).map { a => target / j :: a}
        }.toList
      }
    }
    subProblem(n, n)
  }

  println(print(4))

}
