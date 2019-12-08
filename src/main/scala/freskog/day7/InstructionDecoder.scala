package freskog.day7

import zio.ZIO

trait InstructionDecoder {
  val instructionDecoder: InstructionDecoder.Service[Any]
}

object InstructionDecoder {

  trait Service[R] {
    def decodeAt(p: Position): ZIO[Any, Nothing, Instruction]
  }

  import fastparse._, NoWhitespace._

  def singleDigit[_: P]: P[Int] =
    P(CharPred(_.isDigit).!.map(_.toInt))

  def number[_: P]: P[Int] =
    P(CharsWhile(c => c.isDigit || c == '-').!.map(_.toInt))

  def position[_: P]: P[Position] =
    P(number.map(Position))

  def addr[_: P]: P[Addr] =
    P(position.map(Addr))

  def value[_: P]: P[Value] =
    P(number.map(Value))

  def param[_: P](mode: Int): P[Param] =
    mode match {
      case 0 => addr
      case 1 => value
      case _ => Fail
    }

  def sep[_: P]: P[Unit] =
    P(CharPred(_ == ','))

  def parse3[_: P](m1: Int, m2: Int): P[(Param, Param, Addr)] =
    P(param(m1) ~ sep ~ param(m2) ~ sep ~ addr)

  def add[_: P](m1: Int, m2: Int): P[Add] =
    P(parse3(m1, m2) map Add.tupled)

  def mul[_: P](m1: Int, m2: Int): P[Mul] =
    P(parse3(m1, m2) map Mul.tupled)

  def read[_: P]: P[Read] =
    P(addr map Read)

  def write[_: P](m: Int): P[Write] =
    P(param(m) map Write)

  def jumpIfTrue[_: P](m1: Int, m2: Int): P[JumpIfTrue] =
    P(param(m1) ~ sep ~ param(m2) map JumpIfTrue.tupled)

  def jumpIfFalse[_: P](m1: Int, m2: Int): P[JumpIfFalse] =
    P(param(m1) ~ sep ~ param(m2) map JumpIfFalse.tupled)

  def lessThan[_: P](m1: Int, m2: Int): P[LessThan] =
    P(parse3(m1, m2) map LessThan.tupled)

  def equalTo[_: P](m1: Int, m2: Int): P[EqualTo] =
    P(parse3(m1, m2) map EqualTo.tupled)

  def end[_: P]: P[Instruction] =
    P(Pass(freskog.day7.End))

  def instruction[_: P]: P[Instruction] =
    P((singleDigit ~ singleDigit ~ singleDigit ~ singleDigit ~ singleDigit ~ sep.?).flatMapX {
      case (_, b, a, _, 1) => add(a, b)
      case (_, b, a, _, 2) => mul(a, b)
      case (_, _, _, _, 3) => read
      case (_, _, a, _, 4) => write(a)
      case (_, b, a, _, 5) => jumpIfTrue(a, b)
      case (_, b, a, _, 6) => jumpIfFalse(a, b)
      case (_, b, a, _, 7) => lessThan(a, b)
      case (_, b, a, _, 8) => equalTo(a, b)
      case (_, _, _, 9, 9) => end
    })

  def formatCell(i: Int): String =
    String.format("%05d", i)

  trait Live extends InstructionDecoder {

    val memory: Memory.Service[Any]

    def streamFrom(p: Position): ZIO[Any, Nothing, String] =
      memory.region(p, Instruction.longest).map(_.map(formatCell).mkString(","))

    def decodeInstructionAt(p: Position): ZIO[Any, Nothing, Instruction] =
      streamFrom(p).flatMap(
        parse(_, instruction(_), verboseFailures = true).fold(
          (_, at, extra) => ZIO.dieMessage(s"Parse $p failed: ${extra.trace(true).longMsg} (at $at)"),
          (instruction, _) => ZIO.succeed(instruction)
        )
      )

    override val instructionDecoder: InstructionDecoder.Service[Any] =
      (p: Position) => decodeInstructionAt(p)

  }
}
