package freskog.day19

import freskog.day9.{ Computer, InstructionDecoder, Memory, Position }
import zio._
import zio.console.Console

import scala.annotation.tailrec

object SolutionDay19 extends App {

  def intCode(program: Map[BigInt, BigInt], x: Int, y: Int): ZIO[Any, Nothing, Computer] =
    for {
      posRef         <- Ref.make(Option.empty[Position])
      memRef         <- Ref.make(program)
      relBaseInitial <- Ref.make(Position(0))
      valuesRef      <- Ref.make(List.empty[BigInt])
      inputQueue     <- Queue.bounded[BigInt](2)
      _              <- inputQueue.offerAll(List(BigInt(x), BigInt(y)))
    } yield
      new Computer.Live with Memory.Live with InstructionDecoder.Live with DroneIO with Console.Live {
        override val storage: Ref[Map[BigInt, BigInt]]   = memRef
        override val nextPosition: Ref[Option[Position]] = posRef
        override val relBase: Ref[Position]              = relBaseInitial
        override val input: Queue[BigInt]                = inputQueue
        override val output: Ref[List[BigInt]]           = valuesRef
      }

  def runProgram(program: Map[BigInt, BigInt], x: Int, y: Int): ZIO[Any, Nothing, List[BigInt]] =
    ZIO.accessM[Computer](_.computer.interpreter(Position(0))).provideM(intCode(program, x, y))

  def inTractorBeam(x: Int, y: Int): ZIO[Any, Nothing, Int] =
    for {
      p <- freskog.decodeCommaSeparatedAsMap("freskog/day19/input-day19.txt").orDie
      r <- runProgram(p, x, y)
    } yield r.head.toInt
  
  def findStartRow(row: Int, m: Map[Int, Range]): ZIO[Any, Nothing, Map[Int, Range]] =
    ZIO.foreach(0 to 49)(inTractorBeam(_, row)).flatMap { res =>
      res.count(_ == 1) match {
        case 0 => findStartRow(row + 1, m)
        case 1 => findStartRow(row + 1, m.updated(row, Range.inclusive(res.indexWhere(_ == 1), res.indexWhere(_ == 1))))
        case _ => UIO.succeed(m.updated(row, Range.inclusive(res.indexWhere(_ == 1), res.lastIndexWhere(_ == 1))))
      }
    }

  def firstX(row: Int, prevX: Int): ZIO[Any, Nothing, Int] =
    inTractorBeam(prevX, row).flatMap(n => if (n == 1) UIO.succeed(prevX) else firstX(row, prevX + 1))

  def lastX(row: Int, prevX: Int): ZIO[Any, Nothing, Int] =
    inTractorBeam(prevX, row).flatMap(n => if (n == 0) UIO.succeed(prevX - 1) else lastX(row, prevX + 1))

  def buildRows(rows: Int): ZIO[Any, Nothing, Map[Int, Range]] = {
    def aux(prevRow: Int, m: Map[Int, Range]): UIO[Map[Int, Range]] =
      if (prevRow >= rows) UIO.succeed(m)
      else
        for {
          lowerX    <- firstX(prevRow + 1, m(prevRow).start)
          upperX    <- firstX(prevRow + 1, m(prevRow).end) >>= (lastX(prevRow + 1, _))
          foundRows <- aux(prevRow + 1, m.updated(prevRow + 1, Range.inclusive(lowerX, upperX)))
        } yield foundRows
    findStartRow(0, Map.empty).flatMap(m => aux(m.keys.max, m))
  }

  def render(beam: Map[Int, Range], maxWidth: Option[Int] = None): String =
    (0 to beam.keys.max).map {
      case n if beam.contains(n) => "." * beam(n).start ++ "*" * beam(n).length
      case n                     => "." * n
    }.map(s => maxWidth.fold(s)(maxLen => s.take(maxLen))).mkString("\n")

  val part1: ZIO[Console, Nothing, Unit] =
    buildRows(49)
      .map(render(_, Some(50)))
      .flatMap(input => console.putStrLn(s"part1: points affected = ${input.count(_ == '*')}"))

  @tailrec
  def solve(y: Int, m: Map[Int, Range]): Option[(Int, Int)] =
    if (!m.contains(y + 99)) None
    else if (!m.contains(y)) solve(y + 1, m)
    else if (m(y).end - 99 == m(y + 99).start) Some(m(y).end - 99, y)
    else solve(y + 1, m)

  val part2: ZIO[Console, Nothing, Unit] =
    buildRows(2000).map { m =>
      solve(0, m).map(p => s"part2: $p => answer is ${p._1 * 10000 + p._2}").getOrElse("No solution found")
    }.flatMap(console.putStrLn)

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (part1 *> part2).as(0)
}
