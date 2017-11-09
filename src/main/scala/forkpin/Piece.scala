package forkpin

import scala.util.Try

sealed trait Piece {
  val p: Player
  def toFEN: String
}
case class Pawn(p: Player) extends Piece {
  override def toFEN = if (p == White) "P" else "p"
}
case class Rook(p: Player) extends Piece {
  override def toFEN = if (p == White) "R" else "r"
}
case class Knight(p: Player) extends Piece {
  override def toFEN = if (p == White) "N" else "n"
}
case class Bishop(p: Player) extends Piece {
  override def toFEN = if (p == White) "B" else "b"
}
case class Queen(p: Player) extends Piece {
  override def toFEN = if (p == White) "Q" else "q"
}
case class King(p: Player) extends Piece {
  override def toFEN = if (p == White) "K" else "k"
}

object Piece {
  def fromAN(c: Char): Try[Piece] = Try(c match {
    case 'p' => Pawn(Black)
    case 'r' => Rook(Black)
    case 'n' => Knight(Black)
    case 'b' => Bishop(Black)
    case 'q' => Queen(Black)
    case 'k' => King(Black)
    case 'P' => Pawn(White)
    case 'R' => Rook(White)
    case 'N' => Knight(White)
    case 'B' => Bishop(White)
    case 'Q' => Queen(White)
    case 'K' => King(White)
    case _ => throw BadFEN(s"Piece=$c", "Unknown piece FEN")
  })
}
