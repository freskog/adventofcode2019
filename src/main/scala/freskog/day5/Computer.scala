package freskog.day5

import zio.{ Ref, UIO, ZIO }

trait Computer {
  val computer: Computer.Service[Any]
}

object Computer {

  trait Service[R] {
    def interpreter(pos: Position): ZIO[R, Nothing, List[Int]]
  }

  trait Live extends Computer {

    val memory: Memory.Service[Any]
    val io: IO.Service[Any]
    val instructionDecoder: InstructionDecoder.Service[Any]

    val nextPosition: Ref[Option[Position]]

    override val computer: Service[Any] = new Service[Any] {

      def setJumpPosition(i: Int): ZIO[Any, Nothing, Unit] =
        nextPosition.set(Option(Position(i)))

      def calculateNextPosition(position: Position, inst: Instruction): UIO[Position] =
        nextPosition.get.flatMap {
          case None    => ZIO.succeed(position.skipN(inst.size))
          case Some(p) => nextPosition.set(None) *> ZIO.succeed(p)
        }

      def resolve(p: Param): ZIO[Any, Nothing, Int] =
        p match {
          case Value(v) => ZIO.succeed(v)
          case Addr(a)  => memory.read(a)
        }

      def executeInstruction(instruction: Instruction): ZIO[Any, Nothing, Unit] =
        instruction match {
          case Add(p1, p2, dst)      => (resolve(p1) zipWith resolve(p2))(_ + _) >>= memory.write(dst.position)
          case Mul(p1, p2, dst)      => (resolve(p1) zipWith resolve(p2))(_ * _) >>= memory.write(dst.position)
          case Write(p)              => resolve(p) >>= io.write
          case Read(addr)            => io.read(addr.position)
          case JumpIfTrue(src, dst)  => ZIO.whenM(resolve(src).map(_ != 0))(resolve(dst) >>= setJumpPosition)
          case JumpIfFalse(src, dst) => ZIO.whenM(resolve(src).map(_ == 0))(resolve(dst) >>= setJumpPosition)
          case LessThan(p1, p2, dst) => (resolve(p1) zipWith resolve(p2))(_ < _) >>= memory.writeBool(dst.position)
          case EqualTo(p1, p2, dst)  => (resolve(p1) zipWith resolve(p2))(_ == _) >>= memory.writeBool(dst.position)
          case End                   => ZIO.succeed(())
        }

      override def interpreter(pos: Position): ZIO[Any, Nothing, List[Int]] =
        instructionDecoder.decodeAt(pos).flatMap {
          case End  => io.allWrittenValues
          case inst => executeInstruction(inst) *> calculateNextPosition(pos, inst) >>= interpreter
        }
    }
  }
}
