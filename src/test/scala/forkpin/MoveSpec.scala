package forkpin

import forkpin.Square._
import org.specs2.mutable.Specification

import scala.util.Success

class MoveSpec extends Specification with ArbitraryInput {

  "a valid pawn move" should {
    "be successful" >> {
      (Game.start.move("e4") mustEqual Game.fromFEN("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1"))
        .pendingUntilFixed
    }
  }

  "parsing from algebraic notation" should {
    "parse standard moves with named pieces" >> prop { (t: Token, s: Square) =>
      Move.fromAN(s"${t.toFEN}$s") must beSuccessfulTry[Move]
    }.setGen1(genToken.filter(_.piece != Pawn))

    "sample standard moves with named pieces" >> {
      Move.fromAN("Ke4") mustEqual Success(Move(Token(King, White), e(4)))
      Move.fromAN("qh8") mustEqual Success(Move(Token(Queen, Black), h(8)))
      Move.fromAN("Nb7") mustEqual Success(Move(Token(Knight, White), b(7)))
    }

    //    "parse standard pawn moves" >> prop { (s: Square) =>
    //      Move.fromAN(s) mustEqual
    //    }

  }
}
