package freskog.day9

import freskog.decodeCommaSeparatedAsMap
import zio.{ IO => _, _ }
import zio.console.Console
import zio.stream.ZStream

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

  def phases(start: BigInt, end: BigInt): ZStream[Any, Nothing, (BigInt, BigInt, BigInt, BigInt, BigInt)] =
    ZStream.fromIterator(ZIO.effectTotal((start to end).permutations.map(_.toList).map {
      case p1 :: p2 :: p3 :: p4 :: p5 :: Nil => (p1, p2, p3, p4, p5)
    }))

  def amplifierControllerSoftware(in: Queue[BigInt], out: Queue[BigInt]): ZIO[Any, Nothing, List[BigInt]] =
    in.offer(BigInt(2)) *>
      decodeCommaSeparatedAsMap("freskog/day9/input-day9.txt").orDie >>= (runProgram(_, in, out))

  val partOne: ZIO[Console, Nothing, Unit] =
    (Queue.unbounded[BigInt] zip Queue.unbounded[BigInt]).flatMap {
      case (in, out) => amplifierControllerSoftware(in, out).flatMap(n => console.putStrLn(s"res is $n"))
    }

  val partTwo: ZIO[Console, Nothing, Unit] =
    ZIO.unit

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (partOne *> partTwo).as(0)
}
