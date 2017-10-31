package forkpin

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck

import scala.collection.BitSet

trait ArbitraryInput extends ScalaCheck {

  implicit val arbSquare: Arbitrary[Square] = Arbitrary(genSquare)

  def genSquare: Gen[Square] = for {
    f <- Gen.oneOf('a' to 'h')
    r <- Gen.oneOf(1 to 8)
  } yield Square.parse(s"$f$r").get

  implicit val arbGame: Arbitrary[Game] = Arbitrary(genGame)
  def genGame: Gen[Game] = for {
    pieces <- Gen.listOfN(64, Gen.option(genPiece)).map(_.toArray)
    activePlayer <- Gen.oneOf(Black, White)
    castlingAvailability <- Gen.listOfN(4, Gen.oneOf(true, false)).map(_.zipWithIndex)
      .map(_.foldLeft(BitSet.empty) { case (bits, (on, bit)) => if (on) bits + bit else bits })
    enPassantTarget <- Gen.option(genSquare)
    halfMoveClock <- Gen.choose(0, 50)
    fullMoveClock <- Gen.choose(1, 70)
  } yield Game(pieces, activePlayer, castlingAvailability, enPassantTarget, halfMoveClock, fullMoveClock)

    def genPiece: Gen[Piece] = for {
      player <- Gen.oneOf(Black, White)
      piece <- Gen.oneOf[Player => Piece](King, Queen, Bishop, Knight, Rook, Bishop, Knight, Rook, Pawn, Pawn, Pawn,
        Pawn, Pawn, Pawn, Pawn, Pawn)
    } yield piece(player)


}
