package freskog.day2

import zio._
import freskog._
import zio.console.Console
import zio.stream.ZStream

object SolutionDay2 extends App {

  def env(initial:Array[Int]): UIO[Computer with Memory] =
    ZIO.effectTotal(new Computer.Live with Memory.Live {
      override val input: Array[Int] = Array.copyOf(initial, initial.length)
    })

  val input: ZIO[Any, Nothing, Array[Int]] =
    decodeCommaSeparatedAsArray("freskog/day2/input-day2.txt").orDie

  def processWith(noun: Int, verb: Int): ZIO[Computer with Memory, Nothing, List[Int]] =
    write(Position(1))(noun) *> write(Position(2))(verb) *> interpreter(Position(0))

  val nounAndVerbs: ZStream[Any, Nothing, (Int, Int)] =
    ZStream.fromIterable(0 to 99).cross(ZStream.fromIterable(0 to 99))

  def outputIs(pair:(Int,Int), n:Int): ZIO[Computer with Memory, Nothing, Boolean] =
    processWith(pair._1, pair._2) *> read(Position(0)).map(_ == n)

  def partTwo(input: Array[Int]): ZIO[Console, Nothing, Int] =
    nounAndVerbs.filterM(outputIs(_, 19690720).provideM(env(input))).runHead.flatMap {
      case None               => ZIO.dieMessage("no valid combination found")
      case Some((noun, verb)) => ZIO.succeed((noun, verb))
    }.flatMap (res => console.putStrLn(s"Result is ${100 * res._1 + res._2} (derived from $res)")).as(0)

  def partOne(input: Array[Int]): ZIO[Console, Nothing, Int] =
    processWith(12,2).provideM(env(input)).flatMap {
      res => console.putStrLn(s"Result is ${res.head}")
    }.as(0)

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    input.map(_.toArray) >>= (memory => partOne(memory) *> partTwo(memory))

}
