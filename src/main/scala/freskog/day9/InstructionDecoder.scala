package freskog.day9

import java.math.BigInteger

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
    P(CharPred(_.isDigit).! map (_.toInt))

  def number[_: P]: P[BigInt] =
    P(CharsWhile(c => c.isDigit || c == '-').! map (BigInt(_)))

  def position[_: P]: P[Position] =
    P(number map Position)

  def addr[_: P]: P[Addr] =
    P(position map Addr)

  def value[_: P]: P[Value] =
    P(number map Value)

  def rel[_: P]: P[Rel] =
    P(number map Rel)

  def param[_: P](mode: Int): P[Param] =
    mode match {
      case 0 => addr
      case 1 => value
      case 2 => rel
      case _ => Fail
    }

  def posParam[_: P](mode: Int): P[PosParam] =
    mode match {
      case 0 => addr
      case 2 => rel
      case _ => Fail
    }

  def sep[_: P]: P[Unit] =
    P(CharPred(_ == ','))

  def parse3[_: P](m1: Int, m2: Int, m3: Int): P[(Param, Param, PosParam)] =
    P(param(m1) ~ sep ~ param(m2) ~ sep ~ posParam(m3))

  def add[_: P](m1: Int, m2: Int, m3: Int): P[Add] =
    P(parse3(m1, m2, m3) map Add.tupled)

  def mul[_: P](m1: Int, m2: Int, m3: Int): P[Mul] =
    P(parse3(m1, m2, m3) map Mul.tupled)

  def setRelBase[_: P](m: Int): P[SetRelBase] =
    P(param(m) map SetRelBase)

  def read[_: P](m: Int): P[Read] =
    P(posParam(m) map Read)

  def write[_: P](m: Int): P[Write] =
    P(param(m) map Write)

  def jumpIfTrue[_: P](m1: Int, m2: Int): P[JumpIfTrue] =
    P(param(m1) ~ sep ~ param(m2) map JumpIfTrue.tupled)

  def jumpIfFalse[_: P](m1: Int, m2: Int): P[JumpIfFalse] =
    P(param(m1) ~ sep ~ param(m2) map JumpIfFalse.tupled)

  def lessThan[_: P](m1: Int, m2: Int, m3: Int): P[LessThan] =
    P(parse3(m1, m2, m3) map LessThan.tupled)

  def equalTo[_: P](m1: Int, m2: Int, m3: Int): P[EqualTo] =
    P(parse3(m1, m2, m3) map EqualTo.tupled)

  def end[_: P]: P[Instruction] =
    P(Pass(freskog.day9.End))

  def instruction[_: P]: P[Instruction] =
    P((singleDigit ~ singleDigit ~ singleDigit ~ singleDigit ~ singleDigit ~ sep.?).flatMapX {
      case (c, b, a, _, 1) => add(a, b, c)
      case (c, b, a, _, 2) => mul(a, b, c)
      case (_, _, a, _, 3) => read(a)
      case (_, _, a, _, 4) => write(a)
      case (_, b, a, _, 5) => jumpIfTrue(a, b)
      case (_, b, a, _, 6) => jumpIfFalse(a, b)
      case (c, b, a, _, 7) => lessThan(a, b, c)
      case (c, b, a, _, 8) => equalTo(a, b, c)
      case (_, _, a, 0, 9) => setRelBase(a)
      case (_, _, _, 9, 9) => end
    })

  def formatCell(i: BigInt): String =
    String.format("%05d", new BigInteger(i.toString))

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
