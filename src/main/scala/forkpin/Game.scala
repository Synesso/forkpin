package forkpin

import forkpin.Square._

import scala.annotation.tailrec
import scala.collection.BitSet
import scala.util.Try

object Game {

  val start: Game = fromFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").get

  def fromFEN(fen: String): Try[Game] = {
    Try {
      fen.trim.split(" ") match {
        case Array(placement, activePlayer, castlingAvailability, enPassantTarget, halfMoveClock, fullMoveClock) =>
        val game = Game(
          board = Vector.fill(64)(Option.empty[Piece]),
          activePlayer = activePlayer match {
            case "w" => White
            case "b" => Black
            case _ => throw BadFEN(fen, "invalid active player (field 2)")
          },

          castlingAvailability = castlingAvailability match {
            case "-" => BitSet.empty
            case ca => ca.foldRight(BitSet.empty) {
              case ('K', bs) => bs + 0
              case ('Q', bs) => bs + 1
              case ('k', bs) => bs + 2
              case ('q', bs) => bs + 3
              case _ => throw BadFEN(fen, "invalid castling availability (field 3)")
            }
          },

          enPassantTarget = enPassantTarget match {
            case "-" => None
            case sq => Some(Square.fromAN(sq).getOrElse(throw BadFEN(fen, "invalid enpassant target (field 4)")))
          },

          halfMoveClock = Try(halfMoveClock.toInt)
            .filter(_ >= 0).getOrElse(throw BadFEN(fen, "invalid halfmove clock (field 5)")),

          fullMoveClock = Try(fullMoveClock.toInt)
            .filter(_ >= 1).getOrElse(throw BadFEN(fen, "invalid fullmove clock (field 6)"))
        )
        val ranks = placement.split("/")
        if (ranks.length != 8) throw BadFEN(fen, s"invalid placement (field 1), ${ranks.length} ranks")

        val rankFs = Seq(a _, b _, c _, d _, e _, f _, g _, h _)

        @tailrec
        def parsePlacement(ranks: Seq[String], nextRank: Int, g: Game): Game = ranks match {
          case h +: t =>
            val (placings, gamePlaced) = h.foldLeft((rankFs, g)) {
              case ((fs, gi), c) if c.isDigit =>
                if (fs.size < c.asDigit) throw BadFEN(fen, s"invalid placement (field 1), incorrect qty in rank $h")
                else (fs.drop(c.asDigit), gi)
              case ((fs, gi), c) =>
                (fs.tail, gi.place(Piece.fromAN(c).get, fs.head(nextRank)))
            }
            if (placings.nonEmpty) throw BadFEN(fen, s"invalid placement (field 1), incorrect qty in rank $h")
            parsePlacement(t, nextRank + 1, gamePlaced)
          case _ => g
        }
        Try(parsePlacement(ranks.reverse, 1, game)).get

        case _ =>
          throw BadFEN(fen, "invalid number of fields")
      }
    }
  }
}

case class Game(board: Vector[Option[Piece]],
                activePlayer: Player,
                castlingAvailability: BitSet,
                enPassantTarget: Option[Square],
                halfMoveClock: Int,
                fullMoveClock: Int) {

  def move(algebriacNotation: String): Try[Game] = Move.fromAN(algebriacNotation).flatMap(move)

  private def move(m: Move): Try[Game] = ???


  def toFEN: String = {
    val pieces = board.grouped(8).toSeq.transpose.map { rank =>
      val (row, empty) = rank.foldLeft(("", 0)) {
        case ((s, mt), Some(piece)) if mt > 0 => (s + mt + piece.toFEN, 0)
        case ((s, mt), Some(piece)) => (s + piece.toFEN, 0)
        case ((s, mt), None) => (s, mt + 1)
      }
      if (empty == 0) row else row + empty
    }.reverse.mkString("/")

    val castle = castlingAvailability.map("KQkq".charAt).foldLeft("")(_ + _)

    s"$pieces ${activePlayer.toFEN} $castle ${enPassantTarget.getOrElse("-")} $halfMoveClock $fullMoveClock"
  }

  def pieceAt(sq: Square): Option[Piece] = board(sq.i)

  private[Game] def place(piece: Piece, square: Square): Game = this.copy(board = board.updated(square.i, Some(piece)))

  def canCastle(player: Player, side: Side): Boolean = {
    val bit = (if (player == White) 0 else 2) + (if (side == KingSide) 0 else 1)
    castlingAvailability(bit)
  }
}

case class BadFEN(fen: String, reason: String) extends Exception(s"Could not parse FEN, $reason: $fen")

