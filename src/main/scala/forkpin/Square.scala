package forkpin

import scala.util.Try

case object Square {
  private val NotationRegex = "([a-h])([1-8])".r
  def fromAN(sq: String): Try[Square] = Try {
    val NotationRegex(file, rank) = sq
    Square(((file.head - 'a') * 8) + rank.head - '1')
  }

  def a(i: Int) = Square(i - 1)
  def b(i: Int) = Square(7 + i)
  def c(i: Int) = Square(15 + i)
  def d(i: Int) = Square(23 + i)
  def e(i: Int) = Square(31 + i)
  def f(i: Int) = Square(39 + i)
  def g(i: Int) = Square(47 + i)
  def h(i: Int) = Square(55 + i)

  val all: Set[Square] = (0 to 63).map(Square.apply).toSet
}
 case class Square(i: Int) extends AnyVal {

   override def toString: String = s"${('a' + (i / 8)).asInstanceOf[Char]}${i % 8 + 1}"
   def move(toBlack: Int, toKing: Int): Option[Square] = blackSide(toBlack).flatMap(_.kingSide(toKing))
   def blackSide(dist: Int): Option[Square] = {
     val check = i % 8 + dist
     if (check >= 8 || check < 0) None else Some(Square(i + dist))
   }
   def kingSide(dist: Int): Option[Square] = {
     val check = i + dist * 8
     if (check >= 64 || check < 0) None else Some(Square(i + dist * 8))
   }
   def queenSide(dist: Int): Option[Square] = kingSide(dist * -1)
   def whiteSide(dist: Int): Option[Square] = blackSide(dist * -1)
 }
