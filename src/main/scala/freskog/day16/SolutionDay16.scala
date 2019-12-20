package freskog.day16

import zio._
import zio.console.Console

import scala.annotation.tailrec

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

  @tailrec
  def runPhaseSlice(input:Array[Int], phase:Int):Array[Int] =
    if(phase == 0) input else {
    ((input.length-1) to 0 by -1).foldLeft(0) {
      case (acc, n) =>
        val sum = acc + input(n)
        input(n) = sum % 10 ; sum
    }
    runPhaseSlice(input, phase-1)
  }

  def strToArray(str: String):Array[Int] =
    str.toCharArray.map(c => c.toString.toInt)

  val partOne: ZIO[Console, Nothing, Unit] =
    freskog
      .decodeAsRawString("freskog/day16/input-day16.txt").map(strToArray)
      .orDie
      .map(runPhases(_, 100))
      .flatMap(res => console.putStrLn(s"part one FFT after 100 phases is ${res.take(8).mkString("")}"))


  val partTwo: ZIO[Console, Nothing, Unit] =
    freskog.decodeAsRawString("freskog/day16/input-day16.txt").orDie
    .map(s => s * 10000).map { s =>
      val offset = s.take(7).toInt
      val data = strToArray(s.substring(offset))
      runPhaseSlice(data, 100)
    }.flatMap(
      res => console.putStrLn(s"part two code is ${res.slice(0, 8).mkString("")}")
    )


  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (partOne *> partTwo).as(0)
}
