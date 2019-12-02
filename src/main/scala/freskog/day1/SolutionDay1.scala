package freskog.day1

import java.io.IOException

import freskog._
import zio._

object SolutionDay1 extends App {

  def calculateFuelFromMass(mass: Long): Long =
    (mass / 3L) - 2L

  def calculateFuelForFuel(fuel: Long): Long =
    if (calculateFuelFromMass(fuel) <= 0) fuel
    else calculateFuelForFuel(calculateFuelFromMass(fuel)) + fuel

  val partOne: ZIO[Any, IOException, Long] =
    decodeLines("freskog/day1/input-day1.txt").map(calculateFuelFromMass).fold(0L)(_ + _)

  val partTwo: ZIO[Any, IOException, Long] =
    decodeLines("freskog/day1/input-day1.txt").map(calculateFuelFromMass _ andThen calculateFuelForFuel).fold(0L)(_ + _)

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    ((if (args.isEmpty) partOne else partTwo) >>= ((r: Long) => console.putStrLn(s"res = $r"))).orDie.as(0)

}
