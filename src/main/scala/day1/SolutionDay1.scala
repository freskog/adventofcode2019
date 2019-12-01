package day1

import java.io.{IOException, InputStream}

import zio._
import zio.stream.{ZSink, ZStream}

object SolutionDay1 extends App {

  def openStream(path: String): UIO[InputStream] =
    ZIO.effect(this.getClass.getClassLoader.getResourceAsStream(path)).orDie

  def closeStream(is: InputStream): UIO[Unit] =
    ZIO.effectTotal(is.close())

  def inputStreamFromFile(path: String): Managed[Nothing, InputStream] =
    Managed.make(openStream(path))(closeStream)

  def linesFrom(inputStream: InputStream): ZStream[Any, IOException, Long] =
    ZStream
      .fromInputStream(inputStream)
      .chunks
      .aggregate(ZSink.utf8DecodeChunk)
      .aggregate(ZSink.splitLines)
      .mapConcatChunk(identity)
      .takeUntil(_.isEmpty)
      .map(_.toLong)

  def calculateFuelFromMass(mass: Long): Long =
    (mass / 3L) - 2L

  def calculateFuelForFuel(fuel: Long): Long =
    if (calculateFuelFromMass(fuel) <= 0) fuel
    else calculateFuelForFuel(calculateFuelFromMass(fuel)) + fuel

  def partOne(input: InputStream): ZIO[Any, IOException, Long] =
    linesFrom(input).map(calculateFuelFromMass).fold(0L)(_ + _)

  def partTwo(input: InputStream): ZIO[Any, IOException, Long] =
    linesFrom(input).map(calculateFuelFromMass _ andThen calculateFuelForFuel).fold(0L)(_ + _)

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    inputStreamFromFile("day1/input-day1.txt")
      .use(is => if (args.isEmpty) partOne(is) else partTwo(is))
      .foldM(
        ex => console.putStrLn(s"That didn't work, because $ex"),
        res => console.putStrLn(s"Total fuel is $res")
      )
      .as(0)

}
