import scala.annotation.tailrec

object HaskellFollowAlong {

  def myTake[T](n: Int, xs: List[T]): List[T] = {
    @tailrec
    def loop(acc: List[T], n: Int, rest: List[T]): List[T] = {
      if (n < 0) throw new IllegalArgumentException("negative take")
      else if (n == 0) acc
      else {
        rest match {
          case Nil => acc
          case hd :: tl => loop(hd :: acc, n - 1, tl)
        }
      }
    }
    loop(Nil, n, xs).reverse
  }

}
