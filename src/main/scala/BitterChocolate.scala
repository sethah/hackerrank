object BitterChocolate {
  val memo = new scala.collection.mutable.HashMap[(Boolean, BarState), Boolean]()
  def main(args: Array[String]): Unit = {
    val lines = Seq("asdf", "1 1 1", "2 2 1", "14 12 2").iterator
    //    val lines = io.Source.stdin.getLines()
    lines.next()
    lines.foreach { line =>
      val Array(x, y, z) = line.split(" ").map(_.toInt)
      if (canWin(true, new Bar(Int.MaxValue, (x, y, z)))) println("WIN") else println("LOSE")
    }
  }

  type BarState = (Int, Int, Int)
  class Bar(val length: Int, val state: BarState) {
    def this(length: Int) = this(length, (length, length, length))

    def eat(position: (Int, Int)): Bar = {
      validatePosition(position)
      val (x, y, z) = state
      position match {
        case (1, p) => new Bar(length, (p - 1, y.min(p - 1), z.min(p - 1)))
        case (2, p) => new Bar(length, (x, y.min(p - 1), z.min(p -1)))
        case (3, p) => new Bar(length, (x, y, z.min(p - 1)))
        case (r, p) => throw new IllegalStateException(s"Bad move ($r, $p) for state" +
          s"($x, $y, $z)")
      }
    }

    def validatePosition(position: (Int, Int)): Unit = {
      val (row, col) = position
      val (x, y, z) = state
      assert(Set(1, 2, 3).contains(row))
      assert(col <= Array(x, y, z)(row - 1), s"bad column $col ($row) for state ($x, $y, $z)")
      assert(col > 0, "bar column must not be < 0")
    }

    def isEmpty = state == (0, 0, 0)

    override def toString: String = {
      val s = new StringBuilder()
      s ++= "-" * length + "\n"
      s ++= "*" * state._3 + "\n" + "*" * state._2 + "\n" + "*" * state._1 + "\n"
      s ++= "-" * length
      s.result()
    }
  }

  def canWin(turn: Boolean, bar: Bar): Boolean = {
    if (memo.contains((turn, bar.state))) memo((turn, bar.state))
    bar.state match {
      case (1, 0, 0) => !turn
      case (x, y, z) =>
        val nextMoves = allNextMoves(bar, !turn)
        val wins = nextMoves.map { bar =>
          val cw = if (memo.contains((!turn, bar.state))) memo((!turn, bar.state))
          else canWin(!turn, bar)
          cw
        }
        val res = if (turn) {
          wins.contains(true)
        }
        else {
          wins.forall(_ == true)
        }
        memo += ((turn, bar.state) -> res)
        res
    }
  }

  def allNextMoves(bar: Bar, turn: Boolean): Seq[Bar] = {
    val (x, y, z) = bar.state
    val nextBars = Seq(x, y, z).zipWithIndex.flatMap { case (value, index) =>
      val startCol = if (index == 0) 2 else 1
      val bars = (startCol to value).map(col => bar.eat((index + 1, col)))
      bars
    }
    nextBars//.filter { b => !memo.contains((turn, b.state)) }
  }

}
