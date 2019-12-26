package freskog.day17

case class Vacuum(pos:Pos, facing: Direction) {
  def move(m:Move):Vacuum =
    m match {
      case Forward(n) => Vacuum((1 to n).foldLeft(pos)((p, _) => p.move(facing)), facing)
      case to => Vacuum(pos, facing.turn(to))
    }
}
object Vacuum {
  def from(pos:Pos, c:Char):Vacuum =
    Vacuum(pos, Direction.from(c))
}
