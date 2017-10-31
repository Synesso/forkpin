package forkpin

import forkpin.Square._
import org.scalacheck.Gen
import org.specs2.mutable.Specification

class FENParseSpec extends Specification with ArbitraryInput {

  "parsing a game from FEN" should {
    "be invalid if there are not 6 fields" >> {
      Game.fromFEN("w KQkq - 0 1") must beFailedTry[Game]
      Game.fromFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR KQkq - 0 1") must beFailedTry[Game]
      Game.fromFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - 0 1") must beFailedTry[Game]
      Game.fromFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq 0 1") must beFailedTry[Game]
      Game.fromFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 1") must beFailedTry[Game]
    }

    "field 1 should be the piece placement" >> {
      Game.fromFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") must beSuccessfulTry[Game].like {
        case g: Game =>
          g.pieceAt(a(8)) must beSome[Piece](Rook(Black))
          g.pieceAt(d(8)) must beSome[Piece](Queen(Black))
          g.pieceAt(d(7)) must beSome[Piece](Pawn(Black))
          g.pieceAt(d(6)) must beNone
          g.pieceAt(e(2)) must beSome[Piece](Pawn(White))
          g.pieceAt(a(1)) must beSome[Piece](Rook(White))
          g.pieceAt(f(1)) must beSome[Piece](Bishop(White))
      }
    }

    "field 1 should fail if there are not 8 ranks" >> {
      Game.fromFEN("rnbqkbnr/pppppppp/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") must beFailedTry[Game]
    }

    "field 1 should fail if there are not 8 files per rank" >> {
      Game.fromFEN("rnbqkbnr/pppppppp/6/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") must beFailedTry[Game]
      Game.fromFEN("rnbqkbnr/ppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") must beFailedTry[Game]
      Game.fromFEN("rnbqkbnr/pppppppp/8/8/8/8/PP5PP/RNBQKBNR w KQkq - 0 1") must beFailedTry[Game]
      Game.fromFEN("rnbqkbnr/ppppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") must beFailedTry[Game]
      Game.fromFEN("rnbqkbnr/pppppppp/8/9/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") must beFailedTry[Game]
    }

    "field 1 should fail if there are unparsable characters" >> {
      Game.fromFEN("rnbqkbnr/pppppppp/8/8/8/8/PPxPPPPP/RNBQKBNR w KQkq - 0 1") must beFailedTry[Game]
    }


    "field 2 should set the next move to white or black" >> {
      Game.fromFEN("8/8/8/8/8/8/8/8 w KQkq - 0 1") must beSuccessfulTry[Game].like { case g: Game => g.activePlayer == White }
      Game.fromFEN("8/8/8/8/8/8/8/8 b KQkq - 0 1") must beSuccessfulTry[Game].like { case g: Game => g.activePlayer == Black }
      Game.fromFEN("8/8/8/8/8/8/8/8 a KQkq - 0 1") must beFailedTry[Game]
    }

    "field 3 should set the castling availability" >> prop { (wk: Boolean, wq: Boolean, bk: Boolean, bq: Boolean) =>
      val ca = (if (wk) "K" else "") + (if (wq) "Q" else "") + (if (bk) "k" else "") + (if (bq) "q" else "")
      Game.fromFEN(s"8/8/8/8/8/8/8/8 w ${if (ca == "") "-" else ca } - 0 1") must beSuccessfulTry[Game].like {
        case g: Game =>
          g.canCastle(White, KingSide) mustEqual wk
          g.canCastle(White, QueenSide) mustEqual wq
          g.canCastle(Black, KingSide) mustEqual bk
          g.canCastle(Black, QueenSide) mustEqual bq
      }
    }

    "field 3 should fail when unparsable" >> {
      Game.fromFEN("8/8/8/8/8/8/8/8 w KQkXq - 0 1") must beFailedTry[Game]
    }

    "field 4 should set the en passant target square" >> prop { sq: Square =>
      Game.fromFEN(s"8/8/8/8/8/8/8/8 w KQkq $sq 0 1") must beSuccessfulTry[Game].like {
        case g: Game => g.enPassantTarget must beSome(sq)
      }
    }

    "field 4 should be nothing when '-'" >> {
      Game.fromFEN("8/8/8/8/8/8/8/8 w KQkq - 0 1") must beSuccessfulTry[Game].like {
        case g: Game => g.enPassantTarget must beNone
      }
    }

    "field 4 should fail when unparsable" >> {
      Game.fromFEN("8/8/8/8/8/8/8/8 w KQkq z 0 1") must beFailedTry[Game]
    }

    "field 5 should parse as positive int (or 0)" >> prop { i: Int =>
      Game.fromFEN(s"8/8/8/8/8/8/8/8 w KQkq - $i 1") must beSuccessfulTry[Game].like {
        case g: Game => g.halfMoveClock mustEqual i
      }
    }.setGen(Gen.choose(0, 100))

    "field 5 should fail when negative" >> {
      Game.fromFEN(s"8/8/8/8/8/8/8/8 w KQkq - -1 1") must beFailedTry[Game]
    }

    "field 5 should fail when not a number" >> {
      Game.fromFEN(s"8/8/8/8/8/8/8/8 w KQkq - z 1") must beFailedTry[Game]
    }

    "field 6 should parse as positive int" >> prop { i: Int =>
      Game.fromFEN(s"8/8/8/8/8/8/8/8 w KQkq - 0 $i") must beSuccessfulTry[Game].like {
        case g: Game => g.fullMoveClock mustEqual i
      }
    }.setGen(Gen.choose(1, 100))

    "field 6 should fail when zero" >> {
      Game.fromFEN(s"8/8/8/8/8/8/8/8 w KQkq - 0 0") must beFailedTry[Game]
    }

    "field 6 should fail when negative" >> {
      Game.fromFEN(s"8/8/8/8/8/8/8/8 w KQkq - 0 -1") must beFailedTry[Game]
    }

    "field 6 should fail when not a number" >> {
      Game.fromFEN(s"8/8/8/8/8/8/8/8 w KQkq - 0 z") must beFailedTry[Game]
    }
  }

  "a game serialised to FEN" should {
    "always be restored to the original game" >> prop { game: Game =>
      Game.fromFEN(game.toFEN) must beSuccessfulTry(game)
    }
  }

}
