package freskog.day17

object Tile {
  def from(c: Char): Tile = c match {
    case '#' | '>' | '^' | '<' | 'v' => Scaffold
    case _: Char                     => Space
  }
}

sealed abstract class Tile extends Product with Serializable
case object Scaffold       extends Tile
case object Space          extends Tile
