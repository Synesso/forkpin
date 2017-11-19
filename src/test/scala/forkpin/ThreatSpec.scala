package forkpin

import org.specs2.mutable.Specification
import Square._

class ThreatSpec extends Specification with ArbitraryInput {

  "an empty square" should {
    "not be threatened on an empty board" >> prop { sq: Square =>
      Game.fromFEN("8/8/8/8/8/8/8/8 w KQkq - 0 1").get.threats(sq) must beEmpty
    }
    "not be threatened by a non-adjacent pawn" >> {
      val game = Game.fromFEN("8/pppppppp/8/8/8/8/PPPPPPPP/8 w KQkq - 0 1").get
      val safeSquares = for {
        f <- 'a' to 'h'
        r <- Seq(1, 2, 4, 5, 7, 8)
      } yield Square.fromAN(s"$f$r").get
      forall(safeSquares.map(game.threats)) ((_:Map[Square, Set[Token]]) must beEmpty)
    }
    "be threatened by an adjacent pawn" >> {
      val game = Game.fromFEN("8/pppppppp/8/8/8/8/PPPPPPPP/8 w KQkq - 0 1").get
      game.threats(a(3)) mustEqual Map(b(2) -> Set(Token(Pawn, White)))
      game.threats(b(3)) mustEqual Map(a(2) -> Set(Token(Pawn, White)), c(2) -> Set(Token(Pawn, White)))
      game.threats(c(3)) mustEqual Map(b(2) -> Set(Token(Pawn, White)), d(2) -> Set(Token(Pawn, White)))
      game.threats(d(3)) mustEqual Map(c(2) -> Set(Token(Pawn, White)), e(2) -> Set(Token(Pawn, White)))
      game.threats(e(3)) mustEqual Map(d(2) -> Set(Token(Pawn, White)), f(2) -> Set(Token(Pawn, White)))
      game.threats(f(3)) mustEqual Map(e(2) -> Set(Token(Pawn, White)), g(2) -> Set(Token(Pawn, White)))
      game.threats(g(3)) mustEqual Map(f(2) -> Set(Token(Pawn, White)), h(2) -> Set(Token(Pawn, White)))
      game.threats(h(3)) mustEqual Map(g(2) -> Set(Token(Pawn, White)))
      game.threats(a(6)) mustEqual Map(b(7) -> Set(Token(Pawn, Black)))
      game.threats(b(6)) mustEqual Map(a(7) -> Set(Token(Pawn, Black)), c(7) -> Set(Token(Pawn, Black)))
      game.threats(c(6)) mustEqual Map(b(7) -> Set(Token(Pawn, Black)), d(7) -> Set(Token(Pawn, Black)))
      game.threats(d(6)) mustEqual Map(c(7) -> Set(Token(Pawn, Black)), e(7) -> Set(Token(Pawn, Black)))
      game.threats(e(6)) mustEqual Map(d(7) -> Set(Token(Pawn, Black)), f(7) -> Set(Token(Pawn, Black)))
      game.threats(f(6)) mustEqual Map(e(7) -> Set(Token(Pawn, Black)), g(7) -> Set(Token(Pawn, Black)))
      game.threats(g(6)) mustEqual Map(f(7) -> Set(Token(Pawn, Black)), h(7) -> Set(Token(Pawn, Black)))
      game.threats(h(6)) mustEqual Map(g(7) -> Set(Token(Pawn, Black)))
    }
    "be threatened by an adjacent king" >> {
      val game = Game.fromFEN("8/8/3k4/8/8/4K3/8/8 w KQkq - 0 1").get
      val blackThreats = Set(c(7), d(7), e(7), c(6), e(6), c(5), d(5), e(5))
      val whiteThreats = Set(d(2), e(2), f(2), d(3), f(3), d(4), e(4), f(4))
      val safe = Square.all -- blackThreats -- whiteThreats
      forall(blackThreats.map(game.threats)) {
        (_: Map[Square, Set[Token]]) mustEqual Map(d(6) -> Set(Token(King, Black)))
      }
      forall(whiteThreats.map(game.threats)) {
        (_: Map[Square, Set[Token]]) mustEqual Map(e(3) -> Set(Token(King, White)))
      }
      forall(safe.map(game.threats)) {
        (_: Map[Square, Set[Token]]) must beEmpty
      }
    }
    "be threatened by nearby knights" >> {
      val game = Game.fromFEN("8/8/3n4/8/8/5N2/8/8 w KQkq - 0 1").get
      val blackThreats = Set(c(8), e(8), b(7), f(7), b(5), f(5), c(4), e(4))
      val whiteThreats = Set(e(1), d(2), d(4), e(5), g(1), h(2), h(4), g(5))
      val safe = Square.all -- blackThreats -- whiteThreats
      forall(blackThreats.map(game.threats)) {
        (_: Map[Square, Set[Token]]) mustEqual Map(d(6) -> Set(Token(Knight, Black)))
      }
      forall(whiteThreats.map(game.threats)) {
        (_: Map[Square, Set[Token]]) mustEqual Map(f(3) -> Set(Token(Knight, White)))
      }
      forall(safe.map(game.threats)) {
        (_: Map[Square, Set[Token]]) must beEmpty
      }
    }
    "be threatened by a rook on the same rank or file" >> {
      val game = Game.fromFEN("8/8/2r5/8/8/5R2/8/8 w KQkq - 0 1").get
      val blackThreats = Square.all.filter(sq => sq.toString.startsWith("c") || sq.toString.endsWith("6")) - c(6)
      val whiteThreats = Square.all.filter(sq => sq.toString.startsWith("f") || sq.toString.endsWith("3")) - f(3)
      val safe = Square.all -- blackThreats -- whiteThreats
      forall(blackThreats.map(game.threats)) {
        (_: Map[Square, Set[Token]]) must contain(c(6) -> Set(Token(Rook, Black)))
      }
      forall(whiteThreats.map(game.threats)) {
        (_: Map[Square, Set[Token]]) must contain(f(3) -> Set(Token(Rook, White)))
      }
      forall(safe.map(game.threats)) {
        (_: Map[Square, Set[Token]]) must beEmpty
      }
    }
  }

}
