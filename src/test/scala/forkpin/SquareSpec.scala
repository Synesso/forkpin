package forkpin

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import forkpin.Square._
import org.scalacheck.Gen

import scala.util.Try

class SquareSpec extends Specification with ArbitraryInput {

  "string value of a square" should {
    "mimic its constructor" >> {
      val tests: Seq[(String, String)] = for {
        f <- "abcdefgh" zip List(a _, b _, c _, d _, e _, f _, g _, h _)
        r <- 1 to 8
      } yield (s"${f._1}$r", f._2(r).toString)
      tests.filter{ case (ex, ac) => ex != ac } must beEmpty
    }
  }

  "parsing a square" should {
    "give the correct index" >> {
      val tests: Seq[(Square, Square)] = for {
        f <- "abcdefgh" zip List(a _, b _, c _, d _, e _, f _, g _, h _)
        r <- 1 to 8
      } yield (Square.fromAN(s"${f._1}$r").get, f._2(r))
      tests.filter{ case (ex, ac) => ex != ac } must beEmpty
    }
  }

  "relative squares" should {
    "move kingside" >> {
      f(7).kingSide(-1) must beSome(e(7))
      f(7).kingSide(0) must beSome(f(7))
      f(7).kingSide(1) must beSome(g(7))
      f(7).kingSide(2) must beSome(h(7))
      f(7).kingSide(3) must beNone
    }
    "move queenside" >> {
      c(2).queenSide(-1) must beSome(d(2))
      c(2).queenSide(0) must beSome(c(2))
      c(2).queenSide(1) must beSome(b(2))
      c(2).queenSide(2) must beSome(a(2))
      c(2).queenSide(3) must beNone
    }
    "move blackside" >> {
      d(6).blackSide(-1) must beSome(d(5))
      d(6).blackSide(0) must beSome(d(6))
      d(6).blackSide(1) must beSome(d(7))
      d(6).blackSide(2) must beSome(d(8))
      d(6).blackSide(3) must beNone
    }
    "move whiteside" >> {
      c(3).whiteSide(-1) must beSome(c(4))
      c(3).whiteSide(0) must beSome(c(3))
      c(3).whiteSide(1) must beSome(c(2))
      c(3).whiteSide(2) must beSome(c(1))
      c(3).whiteSide(3) must beNone
    }
    "never fail" >> prop { (s: Square, dist: Int) =>
      Try(s.blackSide(dist)) must beSuccessfulTry
      Try(s.kingSide(dist)) must beSuccessfulTry
      Try(s.queenSide(dist)) must beSuccessfulTry
      Try(s.whiteSide(dist)) must beSuccessfulTry
    }.setGen2(Gen.choose(-8, 8))
  }

}
