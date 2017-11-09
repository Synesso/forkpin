package forkpin

import org.specs2.mutable.Specification

class GameSpec extends Specification {

  "a new game" should {
    "have the pieces set" >> {
      Game.start mustEqual Game.fromFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").get
    }
  }
}
