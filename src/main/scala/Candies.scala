object Candies {

  def main(args: Array[String]): Unit = {
//    val lines = io.Source.stdin.getLines()
//    lines.next()
//    val scores = lines.toArray.map(_.toInt)
//    val scores = Array(4, 4, 3, 4, 2, 1, 2, 2)
//    val scores = List(1, 2, 3, 4, 5, 6, 7).map(_.toLong).reverse
    val scores = (0L until 100000L).toList.reverse
//    val scores = List(2,4,2,6,1,7,8,9,2,1).map(_.toLong)
//    val scores = List(4, 4, 3, 4, 2, 1).map(_.toLong)

//    val candies = nCandies(scores)
    val candies = myCandy(scores, CandySolution(0, Kid(scores.head, 1L), List(Kid(scores.head, 1L))))
    println(candies.nCandy)
//    println(scores.mkString(","))
//    println(candies.mkString(","))
  }

  case class Kid(score: Long, candy: Long)
  case class CandySolution(nCandy: Long, beforeTail: Kid, tail: List[Kid]) {
    // we don't actually need the whole tail...
    def lastScore = tail.headOption.map(_.score).getOrElse(-1L)
    def lastCandy = tail.headOption.map(_.candy).getOrElse(-1L)
    def adjustTail = {
      tail match {
        case Nil => this
        case _ =>
          val lastTail = tail.last
          val tailSize = tail.size
          val tailSum = (1L until tailSize.toLong).sum
          val headSum = if (beforeTail.score == lastTail.score) {
            tail.size.toLong
          } else {
            (beforeTail.candy + 1L).max(tailSize.toLong)
          }
//          println(tailSum, headSum, lastTail.candy, nCandy)
          CandySolution(tailSum + headSum - lastTail.candy + nCandy, lastTail, Nil)
      }
    }
  }

  def myCandy(scores: List[Long], solution: CandySolution): CandySolution = {
    scores match {
      case Nil => {
        println(solution.tail.length)
        solution.adjustTail
      }
      case score :: tl =>
//        println(solution.nCandy, solution.tail.mkString(","), score, solution.lastScore, solution.adjustTail.lastCandy)
        val soln = if (score > solution.lastScore) {
          // simply the previous solution + the last kid's candy + 1
          CandySolution(solution.adjustTail.nCandy + solution.lastCandy + 1, solution.tail.head,
            List(Kid(score, solution.lastCandy + 1L)))
        } else if (score == solution.lastScore) {
          CandySolution(solution.adjustTail.nCandy + 1, solution.tail.head, List(Kid(score, 1L)))
        } else {
          CandySolution(solution.nCandy, solution.beforeTail, Kid(score, 1L) :: solution.tail)
        }
        myCandy(tl, soln)
    }
  }

  def nCandies(scores: List[Long]): Long = {
    def unloadBuffer(buffer: List[Long], last: (Long, Long)): Long = {
      buffer match {
        case Nil => 0L
        case hd :: tl =>
          val tailSum = (1 until buffer.size).sum
          val headSum = if (hd == last._1) buffer.size
          else {
            (last._2 + 1L).max(buffer.size.toLong)
          }
          println(buffer.mkString(","), "|", last, headSum, tailSum)
          headSum + tailSum
      }
    }
    def loop(acc: Long, last: (Long, Long), buffer: List[Long], rest: List[Long]): Long = {
      // what you need to do is as long as you have a string of decreasing numbers, do nothing,
      // but build up a buffer of those values, then when you stop that string, go back and solidify
      // those values then you can discard them
      rest match {
        case Nil => acc + unloadBuffer(buffer, last) // unload the buffer and add last
        case hd :: tl =>
          if (hd > last._1) {
            // don't add in the candy because we'll do it when we empty the buffer
            loop(acc + unloadBuffer(buffer, last), (hd, last._2 + 1L), List(hd), tl)
          } else if (hd == last._1) {
            loop(acc + unloadBuffer(buffer, last), (hd, 1L), List(hd), tl)
          } else {
            loop(acc, last, hd :: buffer, tl)
          }
      }
//      rest match {
//        case Nil => acc + unloadBuffer(buffer, last)
//        case hd :: tl =>
//          if (hd > last._1) {
//            loop(acc + last._2 + 1 + unloadBuffer(buffer, last), (hd, last._2 + 1L),
//              List((hd, last._2 + 1L)), tl)
//          }
//          else if (hd == last._1) {
//            loop(acc + 1L + unloadBuffer(buffer, last), (hd, 1L), List((hd, 1L)), tl)
//          }
//          else {
//            loop(acc, last, (hd, 1L) :: buffer, tl)
//          }
//      }
    }
    loop(1L, (scores.head, 1L), List(1L), scores.tail)
  }

  def numCandies(scores: Array[Long]): Array[Long] = {
    var j = 1
    val candies = new Array[Long](scores.length)
    candies(0) = 1
    while (j < scores.length) {
      if (scores(j) > scores(j - 1)) candies(j) = candies(j - 1) + 1
      else {
        candies(j) = 1
        var i = j
        while (i >= 1 && scores(i) < scores(i - 1) && candies(i - 1) <= candies(i)) {
          candies(i - 1) += 1
          i -= 1
        }
      }
      j += 1
    }
    candies
  }


}
