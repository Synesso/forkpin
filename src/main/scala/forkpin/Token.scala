package forkpin

import scala.util.Try

case class Token(piece: Piece, player: Player) {
  def toFEN: String = player match {
    case Black => piece.toFEN
    case White => piece.toFEN.toUpperCase
  }
}

case object Token {
  def fromAN(c: Char): Try[Token] = Try(c match {
    case 'p' => Token(Pawn, Black)
    case 'r' => Token(Rook, Black)
    case 'n' => Token(Knight, Black)
    case 'b' => Token(Bishop, Black)
    case 'q' => Token(Queen, Black)
    case 'k' => Token(King, Black)
    case 'P' => Token(Pawn, White)
    case 'R' => Token(Rook, White)
    case 'N' => Token(Knight, White)
    case 'B' => Token(Bishop, White)
    case 'Q' => Token(Queen, White)
    case 'K' => Token(King, White)
    case _ => throw BadFEN(s"Piece=$c", "Unknown piece FEN")
  })
}

sealed trait Piece {
  val toFEN: String
}

case object Pawn extends Piece { val toFEN = "p" }
case object Rook extends Piece { val toFEN = "r" }
case object Knight extends Piece { val toFEN = "n" }
case object Bishop extends Piece { val toFEN = "b" }
case object Queen extends Piece { val toFEN = "q" }
case object King extends Piece { val toFEN = "k" }

trait Player {
  val toFEN: String
}
case object Black extends Player {
  val toFEN = "b"
}
case object White extends Player {
  val toFEN = "w"
}
