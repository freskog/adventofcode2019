package freskog.day5

case class Position(i: Int) {
  def skipN(n: Int): Position = Position(i + n)
}