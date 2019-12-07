package freskog.day5

import freskog.decodeCommaSeparatedAsArray
import zio.console.Console
import zio.{ IO => _, _ }

object SolutionDay5 extends App {

  val initialMemory: UIO[Array[Int]] =
    decodeCommaSeparatedAsArray("freskog/day5/input-day5.txt").orDie

  def env(initial: Array[Int], input: List[Int]): UIO[Computer with Memory with InstructionDecoder with IO] =
    for {
      inRef  <- Ref.make(input)
      outRef <- Ref.make(List.empty[Int])
      posRef <- Ref.make(Option.empty[Position])
    } yield
      new Computer.Live with Memory.Live with InstructionDecoder.Live with IO.Live {
        override val storage: Array[Int]                 = Array.copyOf(initial, initial.length)
        override val stdIn: Ref[List[Int]]               = inRef
        override val stdOut: Ref[List[Int]]              = outRef
        override val nextPosition: Ref[Option[Position]] = posRef
      }

  def runProgram(program: Array[Int], input: List[Int]): ZIO[Any, Nothing, List[Int]] =
    ZIO.accessM[Computer](_.computer.interpreter(Position(0))).provideM(env(program, input))

  val partOne: ZIO[Console, Nothing, Unit] =
    initialMemory >>= (runProgram(_, List(1)).map(_.mkString(","))) >>= console.putStrLn

  val partTwo: ZIO[Console, Nothing, Unit] =
    initialMemory >>= (runProgram(_,List(5)).flatMap (code => console.putStrLn(s"Diagnostic code is ${code.head}")))

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (partOne *> partTwo).as(0)

}
