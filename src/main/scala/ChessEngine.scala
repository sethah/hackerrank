object ChessEngine {

//  type Board = Map[(Int, Int), Char]
  sealed trait ChessPiece
  case class Queen(color: Char) extends ChessPiece
  case class Rook(color: Char) extends ChessPiece
  case class Bishop(color: Char) extends ChessPiece
  case class Knight(color: Char) extends ChessPiece
  case object Empty extends ChessPiece

  class Board(val board: Array[Array[ChessPiece]]) {

//    def playerWon: Boolean = pieces.contains('Q') && !oppPieces.contains('Q')
//
//    def getValidMoves(piece: Char, position: (Int, Int)): List[(Int, Int)] = {
//      piece match {
//        case Queen =>
//        case Knight =>
//          List((3, 1), (3, -1), (-3, 1), (-3, -1), (1, 3), (1, -3), (-1, 3), (-1, -3)).map {
//            case (x, y) =>
//              (position._1 + x, position._2 + y)
//          }.filter { case (x, y) => board(x)(y) == Empty}
//        case Rook =>
//        case Bishop =>
//
//      }
//    }

    override def toString: String = {
      board.map { row =>
        row.map {
          case Queen(c) => "Q" + (if (c == 'W') "" else "'")
          case Rook(c) => "R" + (if (c == 'W') "" else "'")
          case Knight(c) => "K" + (if (c == 'W') "" else "'")
          case Bishop(c) => "B" + (if (c == 'W') "" else "'")
          case Empty => "_"
        }.mkString(" ")
      }.mkString("\n")
    }

//    def getNextMoves(turn: Boolean): List[Board] = {
//      if (turn) pieces.map { case (piece, positions) =>
//        new Board()
//      }
//    }
//
//    def canWin(m: Int, turn: Boolean): Boolean = {
//      if (m < 0) false
//      else if (playerWon && turn) true
//      else {
//        val nextMoves = getNextMoves(turn)
//        nextMoves.exists { board => board.canWin(m - 1, !turn)}
//      }
//    }

  }

  def main(args: Array[String]): Unit = {

    val board = new Board(Array(Array(Empty, Empty, Queen('W')), Array(Knight('B'), Bishop('W'), Empty),
      Array(Rook('B'), Empty, Queen('B'))))
    println(board)
  }
}
