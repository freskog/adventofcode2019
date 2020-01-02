package freskog.day20

import zio._
import zio.console.Console

import scala.annotation.tailrec

object SolutionDay20 extends App {

  case class Pos(x: Int, y: Int) {
    def neighbors: List[Pos]           = List(Pos(x + 1, y), Pos(x - 1, y), Pos(x, y + 1), Pos(x, y - 1))
    def above: Pos                     = Pos(x, y - 1)
    def below: Pos                     = Pos(x, y + 1)
    def leftOf: Pos                    = Pos(x - 1, y)
    def rightOf: Pos                   = Pos(x + 1, y)
    def isLeftOf(other: Pos): Boolean  = x < other.x
    def isRightOf(other: Pos): Boolean = x > other.x
    def isAbove(other: Pos): Boolean   = y < other.y
    def isBelow(other: Pos): Boolean   = y > other.y
  }

  sealed abstract class Tile {
    def isLetter: Boolean = this match {
      case Letter(_) => true
      case _         => false
    }
    def isSpace: Boolean = this match {
      case Space => true
      case _     => false
    }
  }
  case object Space          extends Tile
  case object Wall           extends Tile
  case object Hole           extends Tile
  case class Letter(c: Char) extends Tile

  sealed abstract class Warp(val name: String) {
    def dual: Option[Warp] = this match {
      case Inner(name) => Some(Outer(name))
      case Outer(name) => Some(Inner(name))
      case _           => None
    }
  }
  case class Inner(override val name: String) extends Warp(name)
  case class Outer(override val name: String) extends Warp(name)
  case object Goal                            extends Warp("ZZ")
  case object Start                           extends Warp("AA")

  case class World(vert: Set[Pos], edges: Map[Pos, List[Pos]], warps: Map[Pos, Warp])

  def parseTiles(input: String): Map[Pos, Tile] = {
    val (_, _, tiles) = input.foldLeft((0, 0, Map.empty[Pos, Tile])) {
      case ((x, y, vs), '.')  => (x + 1, y, vs.updated(Pos(x, y), Space))
      case ((x, y, vs), '#')  => (x + 1, y, vs.updated(Pos(x, y), Wall))
      case ((x, y, vs), ' ')  => (x + 1, y, vs.updated(Pos(x, y), Hole))
      case ((x, y, vs), '\n') => (0, y + 1, vs)
      case ((x, y, vs), c)    => (x + 1, y, vs.updated(Pos(x, y), Letter(c)))
    }
    tiles
  }

  def parseLetter(p: Pos, tiles: Map[Pos, Tile]): Option[String] =
    tiles.get(p).collect {
      case Letter(c) => c.toString
    }

  def outer(name: String): Warp = name match {
    case "ZZ" => Goal
    case "AA" => Start
    case name => Outer(name)
  }

  def parseWarp(p: Pos, tiles: Map[Pos, Tile]): Option[Warp] =
    for {
      (pn1, la) <- p.neighbors.flatMap(pn1 => parseLetter(pn1, tiles).map(pn1   -> _)).headOption
      (pn2, lb) <- pn1.neighbors.flatMap(pn2 => parseLetter(pn2, tiles).map(pn2 -> _)).headOption
    } yield
      if (pn2.isLeftOf(pn1))
        if (pn2.x == 0) outer(s"$lb$la") else Inner(s"$lb$la")
      else if (pn2.isAbove(pn1))
        if (pn2.y == 0) outer(s"$lb$la") else Inner(s"$lb$la")
      else if (pn2.isRightOf(pn1))
        if (pn2.x == tiles.keys.maxBy(_.x).x) outer(s"$la$lb") else Inner(s"$la$lb")
      else if (pn2.y == tiles.keys.maxBy(_.y).y) outer(s"$la$lb")
      else Inner(s"$la$lb")

  def parseWorld(tiles: Map[Pos, Tile]): World = {
    val (vs, es, ws) = tiles.foldLeft((Set.empty[Pos], Map.empty[Pos, List[Pos]], Map.empty[Pos, Warp])) {
      case ((vs, es, ws), (p, Space)) =>
        (vs + p, es.updated(p, p.neighbors.filter(tiles(_).isSpace)), parseWarp(p, tiles).fold(ws)(ws.updated(p, _)))
      case (acc, _) => acc
    }
    World(vs, es, ws)
  }

  def mergeShortestDistance(m1: Map[Warp, Int], m2: Map[Warp, Int]): Map[Warp, Int] =
    (m1.keys.toSet ++ m2.keys.toSet).foldLeft(Map.empty[Warp, Int]) {
      case (acc, w) => acc.updated(w, math.max(m2.getOrElse(w, m1(w)), m1.getOrElse(w, m2(w))))
    }

  def reachableFrom(p: Pos, d: Int, s: Warp, found: Map[Warp, Int], v: Set[Pos], world: World): Map[Warp, Int] =
    if (world.warps.contains(p) && world.warps(p) != s) mergeShortestDistance(Map(world.warps(p) -> d), found)
    else
      world
        .edges(p)
        .filterNot(v.contains)
        .map(pn => reachableFrom(pn, d + 1, s, found, v + pn, world))
        .foldLeft(found)(mergeShortestDistance)

  def calculateWarpsAndDistances(world: World): (Map[Warp, List[Warp]], Map[(Warp, Warp), Int]) =
    world.warps.keys.foldLeft(Map.empty[Warp, List[Warp]], Map.empty[(Warp, Warp), Int]) {
      case ((edges, dists), p) =>
        val s = world.warps(p)
        val l = reachableFrom(p, 0, s, s.dual.fold(Map.empty[Warp, Int])(w => Map(w -> 1)), Set(p), world)
        (edges.updated(world.warps(p), l.keys.toList), dists ++ l.map(wd => (world.warps(p), wd._1) -> wd._2))
    }

  final def shortestPathBetween[A](source: A, target: A, neighbors: A => List[A], dists: (A, A) => Int): Option[Int] = {

    @tailrec
    def unfold(a: A, prev: Map[A, A], acc: Int): Int =
      if (prev.contains(a)) unfold(prev(a), prev, dists(prev(a), a) + acc) else acc

    @tailrec
    def aux(prev: Map[A, A], visited: Set[A], q: Map[A, Int]): Option[Int] =
      if (q.isEmpty) None
      else
        q.minBy(_._2) match {
          case (`target`, _) => Some(unfold(target, prev, 0))
          case (u, distU) =>
            val q2       = q - u
            val visited2 = visited + u
            val (prev2, q3) = neighbors(u).filterNot(visited.contains).foldLeft((prev, q2)) {
              case ((prev, dist), v) =>
                val alt = distU + dists(u, v)
                if (alt < q2.getOrElse(v, Int.MaxValue)) (prev.updated(v, u), dist.updated(v, alt)) else (prev, dist)
            }
            aux(prev2, visited2, q3)
        }

    aux(Map.empty, Set.empty, Map(source -> 0))
  }

  def distancePart1(input: String): Int = {
    val tiles          = parseTiles(input)
    val world          = parseWorld(tiles)
    val (edges, dists) = calculateWarpsAndDistances(world)
    shortestPathBetween[Warp](Start, Goal, edges(_), (a1: Warp, a2: Warp) => dists(a1, a2)).get
  }

  def neigbors(edges: Map[Warp, List[Warp]])(w: Warp, l: Int): List[(Warp, Int)] =
    edges(w).map(w -> _).flatMap {
      case (Outer(n1), Inner(n2)) if n1 == n2 && l > 0 => Some(Inner(n2), l - 1)
      case (Inner(n1), Outer(n2)) if n1 == n2          => Some(Outer(n2), l + 1)
      case (_, Outer(n2)) if l > 0                     => Some(Outer(n2), l)
      case (_, Inner(n2))                              => Some(Inner(n2), l)
      case (_, Goal) if l == 0                         => Some(Goal, l)
      case (_, _)                                      => None
    }

  def distancePart2(input: String): Int = {
    val tiles          = parseTiles(input)
    val world          = parseWorld(tiles)
    val (edges, dists) = calculateWarpsAndDistances(world)
    shortestPathBetween[(Warp, Int)]((Start, 0),
                                     (Goal, 0),
                                     (neigbors(edges) _).tupled,
                                     (a1: (Warp, Int), a2: (Warp, Int)) => dists(a1._1, a2._1)).get
  }

  val part1: ZIO[Console, Nothing, Unit] =
    freskog
      .decodeAsRawString("freskog/day20/input-day20.txt")
      .orDie
      .map(distancePart1)
      .flatMap(n => console.putStrLn(s"part1: distance is $n"))

  val part2: ZIO[Console, Nothing, Unit] =
    freskog
      .decodeAsRawString("freskog/day20/input-day20.txt")
      .orDie
      .map(distancePart2)
      .flatMap(n => console.putStrLn(s"part2: distance is $n"))

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (part1 *> part2).as(0)
}
