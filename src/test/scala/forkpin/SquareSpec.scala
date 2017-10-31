package forkpin

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import forkpin.Square._

class SquareSpec extends Specification with ScalaCheck {

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
      } yield (Square.parse(s"${f._1}$r").get, f._2(r))
      tests.filter{ case (ex, ac) => ex != ac } must beEmpty
    }
  }

}
