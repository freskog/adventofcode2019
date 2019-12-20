package freskog.day15

sealed abstract class Tile extends Product with Serializable {
  def render: String = this match {
    case Wall   => "\u2588"
    case Empty  => " "
    case Goal   => "G"
    case Oxygen => "O"
  }

}
object Tile {
  def from(i: BigInt): Tile =
    i.toInt match {
      case 0 => Wall
      case 1 => Empty
      case 2 => Goal
      case 3 => Oxygen
    }
}
case object Wall   extends Tile
case object Empty  extends Tile
case object Goal   extends Tile
case object Oxygen extends Tile
