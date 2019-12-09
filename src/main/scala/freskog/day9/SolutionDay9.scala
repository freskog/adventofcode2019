package freskog.day9

import freskog.decodeCommaSeparatedAsMap
import zio.{ IO => _, _ }
import zio.console.Console

object SolutionDay9 extends App {

  def intCode(initial: Map[BigInt, BigInt], in: Queue[BigInt], out: Queue[BigInt]): UIO[Computer] =
    for {
      outRef         <- Ref.make(List.empty[BigInt])
      posRef         <- Ref.make(Option.empty[Position])
      memRef         <- Ref.make(initial)
      relBaseInitial <- Ref.make(Position(0))
    } yield
      new Computer.Live with Memory.Live with InstructionDecoder.Live with IO.Live {
        override val storage: Ref[Map[BigInt, BigInt]]      = memRef
        override val stdIn: Queue[BigInt]                   = in
        override val stdOut: Queue[BigInt]                  = out
        override val copyOfWrittenValues: Ref[List[BigInt]] = outRef
        override val nextPosition: Ref[Option[Position]]    = posRef
        override val relBase: Ref[Position]                 = relBaseInitial
      }

  def runProgram(program: Map[BigInt, BigInt], in: Queue[BigInt], out: Queue[BigInt]): ZIO[Any, Nothing, List[BigInt]] =
    ZIO.accessM[Computer](_.computer.interpreter(Position(0))).provideM(intCode(program, in, out))

  def amplifierControllerSoftware(mode: BigInt): ZIO[Console, Nothing, Unit] =
    for {
      in      <- Queue.unbounded[BigInt]
      out     <- Queue.unbounded[BigInt]
      _       <- in.offer(mode)
      program <- decodeCommaSeparatedAsMap("freskog/day9/input-day9.txt").orDie
      output  <- runProgram(program, in, out)
      _       <- console.putStrLn(s"Mode = $mode, output = [${output.mkString(",")}]")
    } yield ()

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (amplifierControllerSoftware(1) *> amplifierControllerSoftware(2)).as(0)
}
