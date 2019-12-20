package freskog.day15

import zio._
import zio.console.Console

trait Droid {
  val droid: Droid.Service[Any]
}

object Droid {
  trait Service[R] {
    def shortestPathToGoal: ZIO[R, Nothing, List[Movement]]
    def timeToOygenateMap: ZIO[R, Nothing, Int]
  }

  trait Live extends Droid {

    val console: Console.Service[Any]

    val movement: Queue[Movement]
    val status: Queue[Tile]

    val droidPos: Ref[Pos]
    val world: Ref[Map[Pos, Tile]]

    def allValidMoves: ZIO[Any, Nothing, List[Movement]] =
      for {
        w          <- world.get
        p          <- droidPos.get
        directions = List(East, South, West, North).filterNot(m => w.contains(p.move(m)))
        valid      <- ZIO.foreach(directions)(probe).map(_.collect { case (m, Empty | Goal) => m })
      } yield valid

    def allEmptyNeighbors: ZIO[Any, Nothing, List[Movement]] =
      for {
        w <- world.get
        p <- droidPos.get
      } yield List(East, South, West, North).filter(m => w(p.move(m)) == Empty)

    def allPathsToGoal(path: List[Movement]): ZIO[Any, Nothing, List[Movement]] =
      allValidMoves.flatMap(ZIO.foreach(_)(explorePath(_, path)).map(findShortestPath))

    def minutesToFillMap(minutes: Int): ZIO[Any, Nothing, Int] =
      allEmptyNeighbors.flatMap(ZIO.foreach(_)(oxygenatePath(_, minutes + 1))).map(findLongestDuration(minutes, _))

    def findShortestPath(movements: List[List[Movement]]): List[Movement] = {
      val nonEmpty = movements.filter(_.nonEmpty)
      if (nonEmpty.isEmpty) Nil else nonEmpty.minBy(_.size)
    }

    def findLongestDuration(prevDur: Int, allDurations: List[Int]): Int =
      if (allDurations.isEmpty) prevDur else allDurations.max

    def oxygenatePath(m: Movement, minutes: Int): ZIO[Any, Nothing, Int] =
      oxygenate(m) *> minutesToFillMap(minutes) <* backtrack(m)

    def explorePath(m: Movement, path: List[Movement]): ZIO[Any, Nothing, List[Movement]] =
      explore(m).flatMap {
        case Goal => ZIO.succeed(m :: path)
        case _    => allPathsToGoal(m :: path)
      } <* explore(m.reverse)

    def setTileAndPos(m: Movement, tile: Tile): ZIO[Any, Nothing, Map[Pos, Tile]] =
      (tile match {
        case Goal | Empty => droidPos.update(_.move(m))
        case Wall         => droidPos.get.map(_.move(m))
      }).flatMap(p => world.update(w => w.updated(p, w.get(p).fold(tile)(identity))))

    def oxygenate(m: Movement): ZIO[Any, Nothing, Map[Pos, Tile]] =
      droidPos.update(_.move(m)) >>= (p => world.update(_.updated(p, Oxygen)))

    def backtrack(m: Movement): UIO[Pos] =
      droidPos.update(_.move(m.reverse))

    def explore(m: Movement): ZIO[Any, Nothing, Tile] =
      movement.offer(m) *> status.take.tap(setTileAndPos(m, _))

    def probe(m: Movement): ZIO[Any, Nothing, (Movement, Tile)] =
      explore(m).tap {
        case Wall         => ZIO.unit
        case Empty | Goal => explore(m.reverse)
      }.map((m, _))

    def followPath(path:List[Movement]): ZIO[Any, Nothing, List[Pos]] =
      ZIO.foreach(path)(m => droidPos.update(_.move(m)))

    lazy val renderWorld: ZIO[Any, Nothing, Unit] =
      world.get.map { w =>
        val minX   = w.keys.minBy(_.x).x
        val minY   = w.keys.minBy(_.y).y
        val maxX   = w.keys.maxBy(_.x).x
        val maxY   = w.keys.maxBy(_.y).y
        val width  = (maxX - minX) + 1
        val height = maxY - minY + 1
        val array  = Array.fill(height)(Array.fill[Tile](width)(Empty))
        w.foreachEntry((pos, tile) => array(pos.y - minY)(pos.x - minX) = tile)
        array.map(_.map(_.render).mkString("")).reverse.mkString("\n")
      }.flatMap(console.putStrLn)
    
    override val droid: Service[Any] = new Service[Any] {
      override def shortestPathToGoal: ZIO[Any, Nothing, List[Movement]] =
        allPathsToGoal(Nil) <* renderWorld

      override def timeToOygenateMap: ZIO[Any, Nothing, Int] =
        (allPathsToGoal(Nil) >>= followPath) *> minutesToFillMap(0) <* renderWorld
    }
  }
}
