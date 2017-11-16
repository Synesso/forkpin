package forkpin

import scala.util.{Failure, Success, Try}

case class Move(t: Token, to: Square, fromFile: Option[Char] = None) {

}

object Move {

  private val NamedPieceMove = "([kqbnrKQBNR])([a-h][1-8])".r

  def fromAN(an: String): Try[Move] = {
    an match {
      case NamedPieceMove(piece, square) =>
        for {
          p <- Token.fromAN(piece.head)
          s <- Square.fromAN(square)
        } yield Move(p, s)
      case _ => Failure(new Exception("todo"))
    }
  }

}
