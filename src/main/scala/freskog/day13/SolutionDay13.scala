package freskog.day13

import freskog.day13.ArcadeIO.Screen
import freskog.day9.{ Computer, InstructionDecoder, Memory, Position }
import zio._
import zio.console.Console

object SolutionDay13 extends App {

  def intCode(arcadeProg: Map[BigInt, BigInt], screenRef: Ref[Screen], consoleL: Console): ZIO[Any, Nothing, Computer] =
    for {
      posRef         <- Ref.make(Option.empty[Position])
      memRef         <- Ref.make(arcadeProg)
      relBaseInitial <- Ref.make(Position(0))
      partialRef     <- Ref.make(List.empty[BigInt])
      scoreRef       <- Ref.make(BigInt(0))
    } yield
      new Computer.Live with Memory.Live with InstructionDecoder.Live with ArcadeIO {
        override val storage: Ref[Map[BigInt, BigInt]]   = memRef
        override val nextPosition: Ref[Option[Position]] = posRef
        override val relBase: Ref[Position]              = relBaseInitial
        override val partialTile: Ref[List[BigInt]]      = partialRef
        override val screen: Ref[Screen]                 = screenRef
        override val console: Console.Service[Any]       = consoleL.console
        override val score: Ref[BigInt]                  = scoreRef
      }

  def runProgram(program: Map[BigInt, BigInt], init: Ref[Screen]): ZIO[Console, Nothing, List[BigInt]] =
    ZIO
      .environment[Console]
      .flatMap(
        console => ZIO.accessM[Computer](_.computer.interpreter(Position(0))).provideM(intCode(program, init, console))
      )

  val partOne: ZIO[Console, Nothing, Unit] =
    for {
      arcadeProg <- freskog.decodeCommaSeparatedAsMap("freskog/day13/input-day13.txt").orDie
      screen     <- Ref.make(Screen(Map.empty))
      _          <- runProgram(arcadeProg, screen)
      res        <- screen.get.map(_.tiles.values.count(_.isBlock))
      _          <- console.putStrLn(s"$res tiles were blocks")
    } yield ()

  val partTwo: ZIO[Console, Nothing, Unit] =
    for {
      arcadeProgHacked <- freskog.decodeCommaSeparatedAsMap("freskog/day13/input-day13.txt").orDie
      screen           <- Ref.make(Screen(Map.empty))
      score            <- runProgram(arcadeProgHacked.updated(BigInt(0), BigInt(2)), screen)
      _                <- console.putStrLn(s"Done, final score is ${score.head}")
    } yield ()

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (partOne *> partTwo).as(0)
}
