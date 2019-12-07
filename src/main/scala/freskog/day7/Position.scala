package freskog.day7

case class Position(i: Int) {
  def skipN(n: Int): Position = Position(i + n)
}