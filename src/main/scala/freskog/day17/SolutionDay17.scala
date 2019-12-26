package freskog.day17

import freskog.day9.{ Computer, InstructionDecoder, Memory, Position }
import zio._

import zio.console.Console

object SolutionDay17 extends App {

  def intCode(program: Map[BigInt, BigInt], input: String): ZIO[Any, Nothing, Computer] =
    for {
      posRef         <- Ref.make(Option.empty[Position])
      memRef         <- Ref.make(program)
      relBaseInitial <- Ref.make(Position(0))
      valuesRef      <- Ref.make(List.empty[BigInt])
      inputQueue     <- Queue.unbounded[BigInt]
      _              <- inputQueue.offerAll(input.map(c => BigInt(c.toInt)))
    } yield
      new Computer.Live with Memory.Live with InstructionDecoder.Live with AsciiIO with Console.Live {
        override val storage: Ref[Map[BigInt, BigInt]]   = memRef
        override val nextPosition: Ref[Option[Position]] = posRef
        override val relBase: Ref[Position]              = relBaseInitial
        override val allValues: Ref[List[BigInt]]        = valuesRef
        override val input: Queue[BigInt]                = inputQueue
      }

  def runProgram(program: Map[BigInt, BigInt], input: String): ZIO[Any, Nothing, List[BigInt]] =
    ZIO.accessM[Computer](_.computer.interpreter(Position(0))).provideM(intCode(program, input))

  def asString(input: List[BigInt]): String =
    input.map(_.toInt.toChar).mkString("")

  val program: URIO[Any, Map[BigInt, BigInt]] =
    freskog.decodeCommaSeparatedAsMap("freskog/day17/input-day17.txt").orDie

  def enableMode2(program: Map[BigInt, BigInt]): Map[BigInt, BigInt] =
    program.updated(BigInt(0), BigInt(2))

  val exploreScaffold: ZIO[Any, Nothing, World] =
    program.flatMap(runProgram(_, "")).map(asString _ andThen World.from)

  val partOne: ZIO[Console, Nothing, Unit] =
    exploreScaffold.flatMap(n => console.putStrLn(s"part one answer is: ${n.tiles.alignmentParam}"))

  val partTwo: ZIO[Console, Nothing, Unit] =
    for {
      input <- exploreScaffold.map(w => Compress.solve(World.explore(w, Nil).maxBy(_.length)))
      mode2 <- program.map(enableMode2)
      res   <- runProgram(mode2, input.mkString("","\n","\nn\n"))
      _     <- console.putStrLn(s"part two answer is: ${res.last.toLong}")
    } yield ()

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (partOne *> partTwo).as(0)
}
