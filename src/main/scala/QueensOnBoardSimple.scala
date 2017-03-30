object QueensOnBoardSimple {
  /*
    given size n, find all ways to place n queens on the board
   */

  case class SquareBoard(queens: Array[Option[Int]]) {
    val size: Int = queens.length

    def place(row: Int, col: Int): SquareBoard = {
      val newQueens = queens.clone()
      newQueens(col) = Some(row)
      SquareBoard(newQueens)
    }

    def isSafe(row: Int, col: Int): Boolean = {
      // List(1, 3, None, None)
      // go through each queen, check if row ==  row or col == col or |row - row| == |col - col|
      queens.zipWithIndex.forall { case (qr, qc) =>
        qr.map { r =>
          r != row && qc != col && (row - r).abs != (col - qc).abs
        }.getOrElse(true)
      }
    }
  }

  // given a board an a number of queens, place a queen on it, then recurse
  def placeQueens(n: Int, board: SquareBoard): List[SquareBoard] = {
    if (n == board.size) List(board)
    else {
      (0 until board.size).filter(row => board.isSafe(row, n)).flatMap { row =>
        placeQueens(n + 1, board.place(row, n))
      }.toList
    }
  }

  /** Imperative solution */
  def nQueens(n: Int): List[List[Int]] = {
    val placements = Array.fill(n)(-1)
    var allPlacements: List[List[Int]] = Nil
    def loop(col: Int): Unit = {
      if (col == 0) {
        allPlacements = placements.toList :: allPlacements
      }
      (0 until n).filter(isSafe(_, placements)).foreach { row =>
        println(row, col, placements.mkString(","))
        placements(col) = row
        loop(col - 1)
      }
    }
    loop(n)
    allPlacements
  }

  def isSafe(r: Int, queens: Array[Int]): Boolean = {
    true
    // is it safe to place
//    queens.zipWithIndex.forall { case (row, col) =>
//        r != row && c != col && (row - r).abs != (col - c).abs
//    }
  }

}
