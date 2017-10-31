package forkpin

import scala.util.Try

sealed trait Piece {
  val p: Player
}
case class Pawn(p: Player) extends Piece
case class Rook(p: Player) extends Piece
case class Knight(p: Player) extends Piece
case class Bishop(p: Player) extends Piece
case class Queen(p: Player) extends Piece
case class King(p: Player) extends Piece

object Piece {
  def fromFEN(c: Char): Try[Piece] = Try(c match {
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
