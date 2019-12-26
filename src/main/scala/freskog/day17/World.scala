package freskog.day17

case class World(tiles: Tiles, vacuum: Vacuum, visited:Set[Pos]) {
  def vacuumOnScaffold: Boolean =
    tiles.tiles(vacuum.pos) == Scaffold
}

object World {
  def from(input: String): World = {
    val (_, tiles, Some(v)) = input.foldLeft((Pos(0, 0), Map.empty[Pos, Tile], Option.empty[Vacuum])) {
      case ((p, acc, v), '#')  => (p.nextCol, acc.updated(p, Scaffold), v)
      case ((p, acc, v), '.')  => (p.nextCol, acc, v)
      case ((p, acc, v), '\n') => (p.nextRow, acc, v)
      case ((p, acc, _), chr)  => (p.nextCol, acc.updated(p, Scaffold), Some(Vacuum.from(p, chr)))
    }
    World(Tiles(tiles.withDefaultValue(Space)), v, Set.empty)
  }

  def moveVacuum(move: Move, world: World): World =
    World(world.tiles, world.vacuum.move(move), world.visited + world.vacuum.move(move).pos)

  def applyMoves(moves: List[Move], world: World): World =
    if (moves.isEmpty) world else moveVacuum(moves.head, applyMoves(moves.tail, world))

  def safeMoves(moves: List[Move], world: World): Boolean = {
    val w = applyMoves(moves, world)
    (!world.visited.contains(w.vacuum.pos) || world.tiles.intersections.contains(w.vacuum.pos)) && w.vacuumOnScaffold
  }

  @scala.annotation.tailrec
  def safeNumberOfMovesForward(n:Int, prepend:List[Move], world:World):List[Move] =
    if(safeMoves(Forward(n) :: prepend, world)) safeNumberOfMovesForward(n+1, prepend, world)
    else if(n-1 > 0) Forward(n-1) :: prepend
    else Forward(1) :: prepend

  def possibleMoves(world: World): List[List[Move]] =
    List(
      safeNumberOfMovesForward(1, Nil, world),
      safeNumberOfMovesForward(1, List(L), world),
      safeNumberOfMovesForward(1, List(R), world),
    ).filter(safeMoves(_, world))

  def explore(world: World, currentPath: List[Move]): List[List[Move]] = {
    val nextMoves = possibleMoves(world)
    if (nextMoves.isEmpty) List(currentPath.reverse)
    else
      nextMoves.flatMap { moves =>
        explore(applyMoves(moves, world), moves ++ currentPath)
      }
  }

}
