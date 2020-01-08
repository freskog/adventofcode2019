package freskog.day22

import zio._
import zio.console.Console

object SolutionDay22 extends App {

  val part1: ZIO[Console, Nothing, Unit] =
    freskog
      .decodeAsRawString("freskog/day22/input-day22.txt")
      .orDie
      .map(ModularArithmetic.part1(_, 10007))
      .flatMap(n => console.putStrLn(s"part1: card 2019 is now at pos $n"))

  val part2: ZIO[Console, Nothing, Unit] =
    freskog
      .decodeAsRawString("freskog/day22/input-day22.txt")
      .orDie
      .map(ModularArithmetic.part2)
      .flatMap(n => console.putStrLn(s"part2: card 2020 was at pos $n"))

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (part1 *> part2).as(0)
}
