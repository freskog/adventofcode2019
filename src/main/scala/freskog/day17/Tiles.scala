package freskog.day17

case class Tiles(tiles: Map[Pos, Tile]) {

  val intersections: List[Pos] =
    tiles.keys.filter(_.neighboring.forall(tiles(_) == Scaffold)).toList

  val alignmentParam: Int =
    intersections.map(_.aligment).sum

}
