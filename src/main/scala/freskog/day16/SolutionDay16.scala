package freskog.day16

import zio._
import zio.console.Console

object SolutionDay16 extends App {

  val pattern: Chunk[Int] =
    Chunk(0, 1, 0, -1)

  def rep(n: Int): Int => Chunk[Int] =
    i => Chunk.fromArray(Array.fill(n)(i))

  def buildPattern(idx: Int, len: Int): Chunk[Int] = {
    val chunk = pattern.flatMap(rep(idx + 1))
    if (chunk.length >= len) chunk.take(len) else chunk ++ buildPattern(idx, len - chunk.length)
  }

  def buildPatterns(length: Int): Map[Int, Chunk[Int]] =
    (0 until length).map(idx => idx -> buildPattern(idx, length + 1).drop(1).materialize).toMap

  def applyPattern(pattern: Chunk[Int], array: Array[Int]): Int =
    math.abs(pattern.zipWithIndex.fold(0) {
      case (acc, (0, _))   => acc
      case (acc, (n, idx)) => acc + (array(idx) * n)
    } % 10)

  def nextPhase(patterns: Map[Int, Chunk[Int]], input: Array[Int]): Array[Int] =
    Array.iterate(0, input.length)(_ + 1).mapInPlace(idx => applyPattern(patterns(idx), input))

  def runPhases(input: Array[Int], phases: Int): Array[Int] = {
    val patterns = buildPatterns(input.length)
    (0 until phases).foldLeft(input) { case (input, _) => nextPhase(patterns, input) }
  }

  val partOne: ZIO[Console, Nothing, Unit] =
    freskog
      .decodeAsRawString("freskog/day16/input-day16.txt").map(_.toCharArray.map(c => c.toString.toInt))
      .orDie
      .map(runPhases(_, 100))
      .flatMap(res => console.putStrLn(s"FFT after 100 phases is ${res.take(8).mkString("")}"))

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    partOne.as(0)
}
