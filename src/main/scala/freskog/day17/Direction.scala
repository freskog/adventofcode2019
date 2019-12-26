package freskog.day17

object Direction {
  def from(c: Char): Direction =
    c match {
      case '<' => West
      case '^' => North
      case 'v' => South
      case '>' => East
    }
}

sealed abstract class Direction {
  def turn(rightOrLeft: Move): Direction =
    if (rightOrLeft == R) this match {
      case North => East
      case South => West
      case West  => North
      case East  => South
    } else this match {
        case North => West
        case South => East
        case West  => South
        case East  => North
      }

  def opposite:Direction = this match {
    case North => South
    case South => North
    case West => East
    case East => West
  }
}
case object North extends Direction
case object South extends Direction
case object West  extends Direction
case object East  extends Direction
