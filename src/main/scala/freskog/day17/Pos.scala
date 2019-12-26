package freskog.day17

case class Pos(x: Int, y: Int) {
  def neighboring: List[Pos] = List(Pos(x - 1, y), Pos(x + 1, y), Pos(x, y - 1), Pos(x, y + 1))
  def nextRow: Pos           = Pos(0, y - 1)
  def nextCol: Pos           = Pos(x + 1, y)

  def aligment: Int = x * (y * -1)

  def directionTo(other:Pos):Direction =
    if(other.x > x) East
    else if(other.x < x) West
    else if(other.y > y) North
    else South

  def move(direction: Direction):Pos = direction match {
    case North => Pos(x, y + 1)
    case South => Pos(x, y - 1)
    case West => Pos(x - 1, y)
    case East => Pos(x + 1, y)
  }

}
