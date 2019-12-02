package freskog.day2

case class Position(i: Int) {
  def skipN(n: Int): Position = Position(i + n)
}