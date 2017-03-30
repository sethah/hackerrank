object Permutations extends App {

  println(permutations(List("a", "a", "c")))

  def permutations[T](items: List[T]): List[List[T]] = {
    items match {
      case Nil => Nil
      case hd :: Nil => List(List(hd))
      case hd :: tl =>
        items.distinct.flatMap { first =>
          val rest = items.diff(List(first))
          permutations(rest).map(p => first :: p)
        }
    }
  }



}
