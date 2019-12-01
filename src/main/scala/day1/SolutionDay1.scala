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

  def decodeLines(path:String): ZStream[Any, IOException, Long] =
    ZStream.managed(inputStreamFromFile(path)).flatMap(linesFrom)

  def calculateFuelFromMass(mass: Long): Long =
    (mass / 3L) - 2L

  def calculateFuelForFuel(fuel: Long): Long =
    if (calculateFuelFromMass(fuel) <= 0) fuel
    else calculateFuelForFuel(calculateFuelFromMass(fuel)) + fuel

  val partOne: ZIO[Any, IOException, Long] =
    decodeLines("day1/input-day1.txt").map(calculateFuelFromMass).fold(0L)(_ + _)

  val partTwo: ZIO[Any, IOException, Long] =
    decodeLines("day1/input-day1.txt").map(calculateFuelFromMass _ andThen calculateFuelForFuel).fold(0L)(_ + _)

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    ((if(args.isEmpty) partOne else partTwo) >>= ((r:Long) => console.putStrLn(s"res = $r"))).orDie.as(0)


}
