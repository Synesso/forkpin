package forkpin

import forkpin.Game.{kingMoves, knightMoves}
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
          board = Vector.fill(64)(Option.empty[Token]),
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
                (fs.tail, gi.place(Token.fromAN(c).get, fs.head(nextRank)))
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

  private[Game] val kingMoves = for {
    r <- -1 to 1
    f <- -1 to 1
    if !(r == 0 && f == 0)
  } yield (r, f)

  private[Game] val knightMoves = for {
    r <- (-2 to 2).filterNot(_ == 0)
    f <- (-2 to 2).filterNot(_ == 0)
    if math.abs(r) != math.abs(f)
  } yield (r, f)

}

case class Game(board: Vector[Option[Token]],
                activePlayer: Player,
                castlingAvailability: BitSet,
                enPassantTarget: Option[Square],
                halfMoveClock: Int,
                fullMoveClock: Int) {

  def threats(sq: Square): Map[Square, Set[Token]] = {

    def checkStepMoves(moves: Seq[(Int, Int)], Target: Piece) =
      moves
        .flatMap { case (r, f) => sq.move(f, r) }
        .flatMap { attackingSquare =>
          pieceAt(attackingSquare).flatMap {
            case t@Token(Target, _) => Some(attackingSquare -> Set(t))
            case _ => None
          }
        }

    val pawns: Seq[(Square, Set[Token])] =
      Seq(sq.kingSide(1).flatMap(_.blackSide(1)), sq.queenSide(1).flatMap(_.blackSide(1))).flatten
        .filter(sq => pieceAt(sq).contains(Token(Pawn, Black))).map(_ -> Set(Token(Pawn, Black))) ++
        Seq(sq.kingSide(1).flatMap(_.whiteSide(1)), sq.queenSide(1).flatMap(_.whiteSide(1))).flatten
          .filter(sq => pieceAt(sq).contains(Token(Pawn, White))).map(_ -> Set(Token(Pawn, White)))

    val kings: Seq[(Square, Set[Token])] = checkStepMoves(kingMoves, King)

    val knights: Seq[(Square, Set[Token])] = checkStepMoves(knightMoves, Knight)

    (pawns ++ kings ++ knights).foldLeft(Map.empty[Square, Set[Token]].withDefaultValue(Set.empty[Token])) { case (acc, (s, p)) =>
        acc.updated(s, acc(s) ++ p)
    }
  }

  def move(an: String): Try[Game] = Move.fromAN(an).flatMap(move)

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

  def pieceAt(sq: Square): Option[Token] = board(sq.i)

  private[Game] def place(token: Token, square: Square): Game = this.copy(board = board.updated(square.i, Some(token)))

  def canCastle(player: Player, side: Side): Boolean = {
    val bit = (if (player == White) 0 else 2) + (if (side == KingSide) 0 else 1)
    castlingAvailability(bit)
  }
}

case class BadFEN(fen: String, reason: String) extends Exception(s"Could not parse FEN, $reason: $fen")

