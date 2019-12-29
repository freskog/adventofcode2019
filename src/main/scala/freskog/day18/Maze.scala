package freskog.day18

import scala.annotation.tailrec

case class Maze(keys: Map[Pos, Key], gates: Map[Pos, Gate], edges: Map[Pos, List[Pos]], vert: Set[Pos]) {

  /* this should probably use a priority queue/heap instead */
  @tailrec
  final def dijkstra[A](target: A,
                        prev: Map[A, A],
                        length: (A, A) => Int,
                        neighbors: (A, Set[A]) => List[A],
                        q: Map[A, Int]): Option[List[A]] =
    if (q.isEmpty || q.forall(_._2 == Int.MaxValue)) None
    else
      q.minBy(_._2) match {
        case (`target`, _) => Some(unfold(target, prev, Nil))
        case (u, distU) =>
          val q2 = q - u
          val (prev2, q3) = neighbors(u, q2.keySet).foldLeft((prev, q2)) {
            case ((prev, dist), v) =>
              val alt = distU + length(u, v)
              if (alt < q2(v)) (prev.updated(v, u), dist.updated(v, alt)) else (prev, dist)
          }
          dijkstra(target, prev2, length, neighbors, q3)
      }

  @tailrec
  final def unfold[A](a: A, prev: Map[A, A], acc: List[A]): List[A] =
    if (prev.contains(a)) unfold(prev(a), prev, a :: acc) else acc

  def shortestPathBetweenKeys: (List[Key], Int) = {
    val ps    = pathsBetweenKeys
    val ds    = distancesBetweenKeys(ps)
    val rs    = restrictionsBetweenKeys(ps)
    val ks    = keys.values.toSet
    var cache = Map.empty[(Key, Set[Key]), (List[Key],Int)]
    def go(k: Key, collected: Set[Key], path: List[Key]): (List[Key], Int) = {
      def allowed(k1: Key, k2: Key): Boolean =
        rs.contains(k1,k2) && rs(k1, k2).map(_.forKey).forall(collected.contains) && !collected.contains(k2)
      if (collected.size == ks.size) (List(k), 0)
      else if(cache.contains(k, collected)) cache(k, collected)
      else {
        val next = ks.filter(allowed(k, _))
        if (next.isEmpty) (path, Int.MaxValue)
        else {
          val (p,l) = next.map { k2 =>
            val (p,dist) = go(k2, collected + k2, k2 :: path)
            (k :: p, dist + ds(k,k2))
          }.minBy(_._2)
          cache = cache.updated((k,collected),(p,l))
          (p,l)
        }
      }
    }
    go(Key("@"), Set(Key("@")), Key("@") :: Nil)
  }

  def shortestPathPart2(keyNames:List[String]): Int = {
    val ps    = pathsBetweenKeys
    val ds    = distancesBetweenKeys(ps)
    val rs    = restrictionsBetweenKeys(ps)
    val ks    = keys.values.toSet
    var cache = Map.empty[(Set[Key], Set[Key]), Int]
    def go(robots:Set[Key], collected: Set[Key]): Int = {
      def allowed(k1: Key, k2: Key): Boolean =
        rs.contains(k1,k2) && rs(k1, k2).map(_.forKey).forall(collected.contains) && !collected.contains(k2)
      if (collected.size == ks.size) 0
      else if(cache.contains((robots,collected))) cache((robots,collected))
      else {
        val l = robots.map { r =>
          val next = ks.filter(allowed(r, _))
          if (next.isEmpty) Int.MaxValue
          else next.map(r2 => go((robots - r) + r2, collected + r2) + ds(r, r2)).min
        }.min
        cache = cache.updated((robots,collected),l)
        l
      }
    }
    val initial = keyNames.map(Key(_)).toSet
    go(initial, initial)
  }

  def keysReachableFrom(restrictions: Map[(Key, Key), List[Gate]])(k1: Key, remaining: Set[Key]): List[Key] = {
    val collected = keys.values.toSet -- remaining
    keys.values.filter(
      k2 =>
        restrictions.contains(k1, k2) && restrictions(k1, k2)
          .forall(g => collected.contains(Key.forGate(g))) && remaining.contains(k2)
    )
  }.toList

  def pathsBetweenKeys: Map[(Key, Key), List[Pos]] =
    for {
      (p1, k1)  <- keys
      (p2, k2)  <- keys if k1 != k2
      q         = vert.map(_ -> Int.MaxValue).toMap.updated(p1, 0)
      neighbors = (p: Pos, remaining: Set[Pos]) => edges(p).filter(px => remaining.contains(px))
      path <- dijkstra(p2, Map.empty, (_: Pos, _: Pos) => 1, neighbors, q)
    } yield (k1, k2) -> path

  def restrictionsBetweenKeys(keyPaths: Map[(Key, Key), List[Pos]]): Map[(Key, Key), List[Gate]] =
    keyPaths.map { case (k2k, path) => k2k -> restrictionsForPath(path) }

  def restrictionsForPath(path: List[Pos]): List[Gate] =
    path.filter(gates.contains).map(gates(_))

  def distancesBetweenKeys(keyPaths: Map[(Key, Key), List[Pos]]): Map[(Key, Key), Int] =
    keyPaths.map { case (k2k, path) => k2k -> path.length }

}

object Maze {
  def from(input: String): Maze = {
    val (_, _, ps, ks, gs) =
      input.foldLeft((0, 0, Set.empty[Pos], Map.empty[Pos, Key], Map.empty[Pos, Gate])) {
        case ((_, y, ps, ks, gs), '\n')          => (0, y - 1, ps, ks, gs)
        case ((x, y, ps, ks, gs), '#')           => (x + 1, y, ps, ks, gs)
        case ((x, y, ps, ks, gs), '.')           => (x + 1, y, addPos(x, y, ps), ks, gs)
        case ((x, y, ps, ks, gs), c) if isKey(c) => (x + 1, y, addPos(x, y, ps), addKey(x, y, c, ks), gs)
        case ((x, y, ps, ks, gs), c)             => (x + 1, y, addPos(x, y, ps), ks, addGate(x, y, c, gs))
      }
    Maze(
      ks,
      gs,
      ps.foldLeft(Map.empty[Pos, List[Pos]])((acc, p) => acc.updated(p, p.possibleNeighbors.filter(ps.contains))),
      ps
    )
  }

  def isKey(c: Char): Boolean = (c.isLetter && c.isLower) || c.isDigit || c == '@'

  def addKey(x: Int, y: Int, c: Char, keys: Map[Pos, Key]): Map[Pos, Key] =
    keys.updated(Pos(x, y), Key(c.toString))

  def addGate(x: Int, y: Int, c: Char, gates: Map[Pos, Gate]): Map[Pos, Gate] =
    gates.updated(Pos(x, y), Gate(c.toString))

  def addPos(x: Int, y: Int, poses: Set[Pos]): Set[Pos] =
    poses + Pos(x, y)
}

case class Pos(x: Int, y: Int) {

  def up: Pos    = Pos(x, y + 1)
  def down: Pos  = Pos(x, y - 1)
  def left: Pos  = Pos(x - 1, y)
  def right: Pos = Pos(x + 1, y)

  def possibleNeighbors: List[Pos] =
    List(up, down, right, left)
}

case class Key(name: String) extends AnyVal {
  def forGate: Gate = Gate(name.toUpperCase)
}
object Key {
  def forGate(g: Gate): Key = g.forKey
}

case class Gate(name: String) extends AnyVal {
  def forKey: Key = Key(name.toLowerCase)
}
