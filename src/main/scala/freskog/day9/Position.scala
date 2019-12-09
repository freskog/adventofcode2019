package freskog.day9

case class Position(i: BigInt) {
  def skipN(n: BigInt): Position = Position(i + n)
}