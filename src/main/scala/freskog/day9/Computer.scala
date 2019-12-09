package freskog.day9

import zio.{ Ref, ZIO }

trait Computer {
  val computer: Computer.Service[Any]
}

object Computer {

  trait Service[R] {
    def interpreter(pos: Position): ZIO[R, Nothing, List[BigInt]]
  }

  trait Live extends Computer {

    val memory: Memory.Service[Any]
    val io: IO.Service[Any]
    val instructionDecoder: InstructionDecoder.Service[Any]

    val nextPosition: Ref[Option[Position]]
    val relBase: Ref[Position]

    override val computer: Service[Any] = new Service[Any] {

      def setJumpPosition(i: BigInt): ZIO[Any, Nothing, Unit] =
        nextPosition.set(Option(Position(i)))

      def clearJumpPosition: ZIO[Any, Nothing, Unit] =
        nextPosition.set(None)

      def updateRelBase(i: BigInt): ZIO[Any, Nothing, Unit] =
        relBase.update(current => Position(current.i + i)).unit

      def getRelPos(offset: BigInt): ZIO[Any, Nothing, Position] =
        relBase.get.map(p => Position(p.i + offset))

      def calculateNextPosition(position: Position, inst: Instruction): ZIO[Any, Nothing, Position] =
        nextPosition.get.flatMap {
          case None    => ZIO.succeed(position.skipN(inst.size))
          case Some(p) => clearJumpPosition *> ZIO.succeed(p)
        }

      def resolveValue(p: Param): ZIO[Any, Nothing, BigInt] =
        p match {
          case Value(v) => ZIO.succeed(v)
          case Addr(a)  => memory.read(a)
          case Rel(v)   => getRelPos(v) >>= memory.read
        }

      def resolvePos(p: Param): ZIO[Any, Nothing, Position] =
        p match {
          case Value(v) => ZIO.dieMessage(s"Attempted to resolve pos, but given value ($v)")
          case Addr(a)  => ZIO.succeed(a)
          case Rel(v)   => getRelPos(v)
        }

      def executeInstruction(instruction: Instruction): ZIO[Any, Nothing, Unit] =
        instruction match {
          case Add(p1, p2, dst)      => add(p1, p2, dst)
          case Mul(p1, p2, dst)      => mul(p1, p2, dst)
          case Write(p)              => resolveValue(p) >>= io.write
          case Read(dst)             => read(dst)
          case SetRelBase(base)      => setRelBase(base)
          case JumpIfTrue(src, dst)  => jumpIfTrue(src, dst)
          case JumpIfFalse(src, dst) => jumpIfFalse(src, dst)
          case LessThan(p1, p2, dst) => lessThan(p1, p2, dst)
          case EqualTo(p1, p2, dst)  => equalTo(p1, p2, dst)
          case End                   => end
        }

      def lessThan(p1: Param, p2: Param, dst: Param): ZIO[Any, Nothing, Unit] =
        (resolveValue(p1) zipWith resolveValue(p2))(_ < _).flatMap {
          bool => resolvePos(dst).flatMap(p => memory.writeBool(p)(bool))
        }

      def add(p1: Param, p2: Param, dst: Param): ZIO[Any, Nothing, Unit] =
        (resolveValue(p1) zipWith resolveValue(p2))(_ + _).flatMap {
          res => resolvePos(dst).flatMap(p => memory.write(p)(res))
        }

      def mul(p1: Param, p2: Param, dst: Param): ZIO[Any, Nothing, Unit] =
        (resolveValue(p1) zipWith resolveValue(p2))(_ * _).flatMap {
          res => resolvePos(dst).flatMap(p => memory.write(p)(res))
        }

      def read(dst: Param): ZIO[Any, Nothing, Unit] =
        resolvePos(dst) >>= io.read

      def setRelBase(base: Param): ZIO[Any, Nothing, Unit] =
        resolveValue(base) >>= updateRelBase

      def equalTo(p1: Param, p2: Param, dst: Param): ZIO[Any, Nothing, Unit] =
        (resolveValue(p1) zipWith resolveValue(p2))(_ == _).flatMap {
          bool => resolvePos(dst).flatMap(p => memory.writeBool(p)(bool))
        }

      def jumpIfFalse(src: Param, dst: Param): ZIO[Any, Nothing, Unit] =
        ZIO.whenM(resolveValue(src).map(_ == BigInt(0)))(resolveValue(dst) >>= setJumpPosition)

      def jumpIfTrue(src: Param, dst: Param): ZIO[Any, Nothing, Unit] =
        ZIO.whenM(resolveValue(src).map(_ != BigInt(0)))(resolveValue(dst) >>= setJumpPosition)

      override def interpreter(pos: Position): ZIO[Any, Nothing, List[BigInt]] =
        instructionDecoder.decodeAt(pos).flatMap {
          case End  => io.allWrittenValues
          case inst => executeInstruction(inst) *> calculateNextPosition(pos, inst) >>= interpreter
        }
    }
  }

  private def end =
    ZIO.succeed(())
}
