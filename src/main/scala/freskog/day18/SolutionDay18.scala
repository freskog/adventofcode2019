package freskog.day18

import zio._
import zio.console.Console

object SolutionDay18 extends App {

  /**
   * Warning: this will take a while to run for each part (though no more than 1 minute on a decent machine)
   */

  val part1: ZIO[Console, Nothing, Unit] =
    freskog.decodeAsRawString("freskog/day18/input-day18-part1.txt")
    .orDie
    .flatMap(input => console.putStrLn(s"p1: ${Maze.from(input).shortestPathPart2(List("@"))}, steps"))

  val part2: ZIO[Console, Nothing, Unit] =
    freskog.decodeAsRawString("freskog/day18/input-day18-part2.txt").orDie
      .flatMap(input => console.putStrLn(s"p2: ${Maze.from(input).shortestPathPart2(List("1","2","3","4"))}, steps"))

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (part1 *> part2).as(0)

}
