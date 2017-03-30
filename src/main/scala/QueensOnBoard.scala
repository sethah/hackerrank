object QueensOnBoard {

  case class Board(val numRows: Int,
                   val numCols: Int,
                   val queens: Set[(Int, Int)]) {

    def place(r: Int, c: Int): Board = {
      new Board(numRows, numCols, queens + ((r, c)))
    }

    def spaces(blocked: Set[(Int, Int)]): List[(Int, Int)] = {
      val attacked = scala.collection.mutable.Set[(Int, Int)]()
      queens.foreach { case (qr, qc) =>
        (qr - 1 to 0 by -1).map((_, qc)).takeWhile { case (r, c) =>
          !blocked.contains((r, c))
        }.foreach(attacked.add)
        (qr + 1 until numCols).map((_, qc)).takeWhile { case (r, c) =>
          !blocked.contains((r, c))
        }.foreach(attacked.add)
        (qc - 1 to 0 by -1).map((qr, _)).takeWhile { case (r, c) =>
          !blocked.contains((r, c))
        }.foreach(attacked.add)
        (qc + 1 until numCols).map((qr, _)).takeWhile { case (r, c) =>
          !blocked.contains((r, c))
        }.foreach(attacked.add)
        Stream.from(1, 1).map(i => (qr + i, qc + i)).takeWhile { case (r, c) =>
          r < numRows && c < numCols && !blocked.contains((r, c))
        }.foreach(attacked.add)
        Stream.from(1, 1).map(i => (qr - i, qc - i)).takeWhile { case (r, c) =>
          r >= 0 && c >= 0 && !blocked.contains((r, c))
        }.foreach(attacked.add)
        Stream.from(1, 1).map(i => (qr - i, qc + i)).takeWhile { case (r, c) =>
          r >= 0 && c < numCols && !blocked.contains((r, c))
        }.foreach(attacked.add)
        Stream.from(1, 1).map(i => (qr + i, qc - i)).takeWhile { case (r, c) =>
          r < numRows && c >= 0 && !blocked.contains((r, c))
        }.foreach(attacked.add)
      }
      val open = for (r <- 0 until numRows;
           c <- 0 until numCols;
           if !attacked.contains((r, c)) && !blocked.contains((r, c))
             && !queens.contains((r, c))) yield {(r, c)}
      open.toList
    }

  }

  def ways2(m: Int, n: Int, blocked: Set[(Int, Int)]): Int = {
    // for open space in open spaces, place a queen on it, update open spaces and recurse
    def loop(acc: Int, toVisit: Set[Board]): Int = {
      if (toVisit.isEmpty) acc
      else {
        val nextBoards = toVisit.flatMap { board =>
          board.spaces(blocked).map { case (r, c) => board.place(r, c)}
        }
        loop(acc + nextBoards.size, nextBoards)
      }
    }
    loop(0, Set(new Board(m, n, Set())))
  }
  def ways(m: Int, n: Int, blocked: Set[(Int, Int)]): Set[Set[(Int, Int)]] = {
    def loop(acc: Int, toVisit: Set[Set[(Int, Int)]],
             visited: Set[Set[(Int, Int)]]): Set[Set[(Int, Int)]] = {
      if (toVisit.isEmpty) visited
      else {
        val next = toVisit.flatMap { board =>
          nextVisits(m, n, board, blocked).toSet
        }
        loop(acc, next, visited ++ next)
      }

    }
    loop(0, Set(Set()), Set())
  }

  def nextVisits(m: Int, n: Int, board: Set[(Int, Int)],
                 blocked: Set[(Int, Int)]): List[Set[(Int, Int)]] = {
    val tmp = for (col <- 0 until n;
                   row <- 0 until m;
                   if !board.contains((row, col));
                   if isSafe2(row, col, m, n, board, blocked)) yield {
      board + ((row, col))
    }
    tmp.toList
  }

  def isSafe(row: Int, col: Int, m: Int, n: Int,
             board: List[Int], blocked: Set[(Int, Int)]): Boolean = {
    val unBlocked = !blocked.contains((row, col))
    val canPlace = board.zipWithIndex.forall { case (value, idx) =>
      (idx != col && value != row && diagonalsAreClear(board, m, n, row, col)) || value == -1
    }
    unBlocked && canPlace
  }

  def isSafe2(row: Int, col: Int, m: Int, n: Int,
             board: Set[(Int, Int)], blocked: Set[(Int, Int)]): Boolean = {
    val unBlocked = !blocked.contains((row, col))
    if (!unBlocked) false
    else {
      board.forall { case (r, c) =>
        canPlaceRow(row, col, r, c, blocked) && canPlaceCol(row, col, r, c, blocked) &&
          canPlaceDiag(row, col, r, c, blocked)
      }
    }
  }

  def canPlaceRow(r1: Int, c1: Int, r2: Int, c2: Int, blocked: Set[(Int, Int)]): Boolean = {
    if (r1 != r2) true
    else {
      (c1.min(c2) + 1 until c1.max(c2)).exists(i => blocked.contains(r1, i))
    }
  }

  def canPlaceCol(r1: Int, c1: Int, r2: Int, c2: Int, blocked: Set[(Int, Int)]): Boolean = {
    if (c1 != c2) true
    else {
      (r1.min(r2) + 1 until r1.max(r2)).exists(i => blocked.contains(i, c1))
    }
  }

  def canPlaceDiag(r1: Int, c1: Int, r2: Int, c2: Int, blocked: Set[(Int, Int)]): Boolean = {
    val sameDiag = (r1 - r2).abs == (c1 - c2).abs
    if (!sameDiag) true
    else {
      if (c1 < c2) {
        (c1 + 1 until c2).exists { i =>
          val pr = if (r1 < r2) r1 + i else r1 - i
          blocked.contains(c1 + i, pr)
        }
      } else {
        (c2 + 1 until c1).exists { i =>
          val pr = if (r2 < r1) r2 + i else r2 - i
          blocked.contains(c2 + i, pr)
        }
      }
    }
  }

  def diagonalsAreClear(board: List[Int], m: Int, n: Int, r: Int, c: Int): Boolean = {
    board.zipWithIndex.forall { case (value, idx) =>
      value == -1 || (idx - c).abs != (value - r).abs
    }
  }

}
