package freskog.day2

import java.io.IOException

import zio._
import freskog._
import zio.stream.ZStream

object SolutionDay2 extends App {

  case class Position(i: Int) {
    def skipN(n: Int): Position = Position(i + n)
  }

  sealed abstract class Instruction                              extends Product with Serializable
  case class Add(src1: Position, src2: Position, dest: Position) extends Instruction
  case class Mul(src1: Position, src2: Position, dest: Position) extends Instruction
  case object End                                                extends Instruction

  trait Memory {
    def write(p: Position)(v: Int): ZIO[Any, Nothing, Unit]
    def read(p: Position): ZIO[Any, Nothing, Int]
    def asList: ZIO[Any, Nothing, List[Int]]
  }

  def read(p: Position): ZIO[Memory, Nothing, Int] =
    ZIO.accessM[Memory](_.read(p))

  def write(p: Position)(v: Int): ZIO[Memory, Nothing, Unit] =
    ZIO.accessM[Memory](_.write(p)(v))

  def asList: ZIO[Memory, Nothing, List[Int]] =
    ZIO.accessM[Memory](_.asList)

  def memory(orig: Array[Int]): ZIO[Any, Nothing, Memory] =
    ZIO.effectTotal(Array.copyOf(orig, orig.length)) >>= { array =>
      ZIO.succeed(new Memory {
        override def write(p: Position)(v: Int): ZIO[Any, Nothing, Unit] =
          ZIO.effectTotal(array(p.i) = v)

        override def read(p: Position): ZIO[Any, Nothing, Int] =
          ZIO.effectTotal(array(p.i))

        override def asList: ZIO[Any, Nothing, List[Int]] =
          ZIO.effectTotal(array.toList)
      })
    }

  def decodePositionAt(position: Position): ZIO[Memory, Nothing, Position] =
    read(position).map(l => Position(l))

  def decode3PositionsAt(position: Position): ZIO[Memory, Nothing, (Position, Position, Position)] =
    (decodePositionAt(position.skipN(1)) <*>
      decodePositionAt(position.skipN(2)) <*>
      decodePositionAt(position.skipN(3))) map {
      case pos1 <*> pos2 <*> pos3 => (pos1, pos2, pos3)
    }

  def decodeAddAt(position: Position): ZIO[Memory, Nothing, Add] =
    decode3PositionsAt(position).map((Add.apply _).tupled)

  def decodeMulAt(position: Position): ZIO[Memory, Nothing, Mul] =
    decode3PositionsAt(position).map((Mul.apply _).tupled)

  def decodeInstruction(position: Position): ZIO[Memory, Nothing, Instruction] =
    read(position).flatMap {
      case 1L    => decodeAddAt(position)
      case 2L    => decodeMulAt(position)
      case 99L   => ZIO.succeed(End)
      case other => ZIO.dieMessage(s"encountered invalid opcode $other")
    }

  def calculateNextPosition(position: Position): UIO[Position] =
    ZIO.succeed(position.skipN(4))

  def executeInstruction(instruction: Instruction): ZIO[Memory, Nothing, Unit] =
    instruction match {
      case Add(src1, src2, dest) => (read(src1) zipWith read(src2))(_ + _) >>= write(dest)
      case Mul(src1, src2, dest) => (read(src1) zipWith read(src2))(_ * _) >>= write(dest)
      case End                   => ZIO.succeed(())
    }

  def interpreter(pos: Position): ZIO[Memory, Nothing, Memory] =
    decodeInstruction(pos).flatMap {
      case End  => ZIO.environment[Memory]
      case inst => executeInstruction(inst) *> calculateNextPosition(pos) >>= interpreter
    }

  def preprocessPartOne: ZIO[Memory, Nothing, Unit] =
    write(Position(1))(12) *> write(Position(2))(2)

  def partOne(input: Array[Int]): ZIO[Any, Nothing, List[Int]] =
    (preprocessPartOne *> interpreter(Position(0)) *> asList).provideM(memory(input))

  val nounAndVerbs: ZStream[Any, Nothing, (Int, Int)] =
    ZStream.fromIterable(0 to 99).cross(ZStream.fromIterable(0 to 99))

  def processPartTwo(noun: Int, verb: Int): ZIO[Memory, Nothing, Boolean] =
    write(Position(1))(noun) *>
      write(Position(2))(verb) *>
      interpreter(Position(0)) *> read(Position(0)).map(_ == 19690720)

  def partTwo(input: Array[Int]): ZIO[Any, Nothing, (Int, Int)] =
    nounAndVerbs.filterM((processPartTwo _).tupled(_).provideM(memory(input))).runHead.flatMap {
      case None               => ZIO.dieMessage("no valid combination found")
      case Some((noun, verb)) => ZIO.succeed((noun, verb))
    }

  val input: ZIO[Any, IOException, Array[Int]] =
    decodeCommaSeparatedAsArray("freskog/day2/input-day2.txt")

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (input.map(_.toArray) >>= partTwo)
      .foldM(
        ex => console.putStrLn(s"Error $ex"),
        res => console.putStrLn(s"Result is ${100 * res._1 + res._2} (derived from $res)")
      )
      .as(0)
}
