package freskog.day20

import zio._

import scala.annotation.tailrec

object SolutionDay20 extends App {

  case class Pos(x: Int, y: Int) {
    def neighbors: List[Pos] =
      List(Pos(x + 1, y), Pos(x - 1, y), Pos(x, y + 1), Pos(x, y - 1))
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
  case class Letter(c: Char) extends Tile

  case class World(vert: Set[Pos], edges: Map[Pos, List[Pos]], warps: Map[Pos, String])

  def parseTiles(input: String): Map[Pos, Tile] = {
    val (_, _, tiles) = input.foldLeft((0, 0, Map.empty[Pos, Tile])) {
      case ((x, y, vs), '.')       => (x + 1, y, vs.updated(Pos(x, y), Space))
      case ((x, y, vs), '#' | ' ') => (x + 1, y, vs.updated(Pos(x, y), Wall))
      case ((x, y, vs), '\n')      => (0, y + 1, vs)
      case ((x, y, vs), c)         => (x + 1, y, vs.updated(Pos(x, y), Letter(c)))
    }
    tiles
  }

  def parseLetter(p: Pos, tiles: Map[Pos, Tile]): Option[String] =
    tiles.get(p).collect {
      case Letter(c) => c.toString
    }

  def parseWarp(p: Pos, tiles: Map[Pos, Tile]): Option[String] =
    for {
      (pn, l2) <- p.neighbors.flatMap(pn => parseLetter(pn, tiles).map(pn -> _)).headOption
      l1       <- pn.neighbors.flatMap(parseLetter(_, tiles)).headOption
    } yield if (pn.x < p.x || pn.y < p.y) s"$l1$l2" else s"$l2$l1"

  def joinWarp(p: Pos, edges: Map[Pos, List[Pos]], warps: Map[Pos, String]): Map[Pos, List[Pos]] =
    warps.find { case (pn, n) => pn != p && n == warps(p) }.fold(edges)(pn => edges.updated(p, pn._1 :: edges(p)))

  def parseWorld(tiles: Map[Pos, Tile]): World = {
    val (vs, es, ws) = tiles.foldLeft((Set.empty[Pos], Map.empty[Pos, List[Pos]], Map.empty[Pos, String])) {
      case ((vs, es, ws), (p, Space)) =>
        (vs + p, es.updated(p, p.neighbors.filter(tiles(_).isSpace)), parseWarp(p, tiles).fold(ws)(ws.updated(p, _)))
      case (acc, _) => acc
    }
    World(vs, ws.keys.foldLeft(es) { case (acc, p) => joinWarp(p, acc, ws) }, ws)
  }

  def findStart(world: World): Pos =
    world.warps.find(_._2 == "AA").head._1

  def findGoal(world: World): Pos =
    world.warps.find(_._2 == "ZZ").head._1

  def distance(p1: Pos, p2: Pos, edges: Map[Pos, List[Pos]]): Int =
    dijkstra(p2, Map.empty, edges, edges.keySet.map( _ -> Int.MaxValue).toMap.updated(p1, 0)).get.length

  /* this should probably use a priority queue/heap instead */
  @tailrec
  final def dijkstra[A](target: A,
                        prev: Map[A, A],
                        edges: (A) => List[A],
                        q: Map[A, Int]): Option[List[A]] =
    if (q.isEmpty || q.forall(_._2 == Int.MaxValue)) None
    else
      q.minBy(_._2) match {
        case (`target`, _) => Some(unfold(target, prev, Nil))
        case (u, distU) =>
          val q2 = q - u
          val (prev2, q3) = edges(u).filter(q2.keySet.contains).foldLeft((prev, q2)) {
            case ((prev, dist), v) =>
              val alt = distU + 1
              if (alt < q2(v)) (prev.updated(v, u), dist.updated(v, alt)) else (prev, dist)
          }
          dijkstra(target, prev2, edges, q3)
      }

  @tailrec
  final def unfold[A](a: A, prev: Map[A, A], acc: List[A]): List[A] =
    if (prev.contains(a)) unfold(prev(a), prev, a :: acc) else acc


  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = ???
}
