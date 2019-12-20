package freskog.day15

case class Pos(x: Int, y: Int) {
  def move(m:Movement):Pos =
    m match {
      case North => Pos(x, y + 1)
      case South => Pos(x, y - 1)
      case West => Pos(x - 1, y)
      case East => Pos(x + 1, y)
    }
}
