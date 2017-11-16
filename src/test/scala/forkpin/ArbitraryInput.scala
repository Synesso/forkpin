package forkpin

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck

import scala.collection.BitSet

trait ArbitraryInput extends ScalaCheck {

  implicit val arbSquare: Arbitrary[Square] = Arbitrary(genSquare)

  def genSquare: Gen[Square] = for {
    f <- Gen.oneOf('a' to 'h')
    r <- Gen.oneOf(1 to 8)
  } yield Square.fromAN(s"$f$r").get

  implicit val arbGame: Arbitrary[Game] = Arbitrary(genGame)
  def genGame: Gen[Game] = for {
    tokens <- Gen.listOfN(64, Gen.option(genToken)).map(_.toVector)
    activePlayer <- Gen.oneOf(Black, White)
    castlingAvailability <- Gen.listOfN(4, Gen.oneOf(true, false)).map(_.zipWithIndex)
      .map(_.foldLeft(BitSet.empty) { case (bits, (on, bit)) => if (on) bits + bit else bits })
    enPassantTarget <- Gen.option(genSquare)
    halfMoveClock <- Gen.choose(0, 50)
    fullMoveClock <- Gen.choose(1, 70)
  } yield Game(tokens, activePlayer, castlingAvailability, enPassantTarget, halfMoveClock, fullMoveClock)

  implicit val arbToken: Arbitrary[Token] = Arbitrary(genToken)
  def genToken: Gen[Token] = for {
    player <- Gen.oneOf(Black, White)
    piece <- Gen.oneOf(King, Queen, Bishop, Knight, Rook, Bishop, Knight, Rook, Pawn, Pawn, Pawn,
      Pawn, Pawn, Pawn, Pawn, Pawn)
  } yield Token(piece, player)

}
