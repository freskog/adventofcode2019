package freskog.day2

import zio.{ <*>, UIO, ZIO }

trait Computer {
  val computer: Computer.Service[Any]
}

object Computer {

  trait Service[R] {
    def interpreter(pos: Position): ZIO[R, Nothing, List[Int]]
  }

  trait Live extends Computer {

    val memory: Memory.Service[Any]

    override val computer: Service[Any] = new Service[Any] {

      def decodePositionAt(position: Position): ZIO[Any, Nothing, Position] =
        memory.read(position).map(l => Position(l))

      def decode3PositionsAt(position: Position): ZIO[Any, Nothing, (Position, Position, Position)] =
        (decodePositionAt(position.skipN(1)) <*>
          decodePositionAt(position.skipN(2)) <*>
          decodePositionAt(position.skipN(3))) map {
          case pos1 <*> pos2 <*> pos3 => (pos1, pos2, pos3)
        }

      def decodeAddAt(position: Position): ZIO[Any, Nothing, Add] =
        decode3PositionsAt(position).map((Add.apply _).tupled)

      def decodeMulAt(position: Position): ZIO[Any, Nothing, Mul] =
        decode3PositionsAt(position).map((Mul.apply _).tupled)

      def decodeInstruction(position: Position): ZIO[Any, Nothing, Instruction] =
        memory.read(position).flatMap {
          case 1L    => decodeAddAt(position)
          case 2L    => decodeMulAt(position)
          case 99L   => ZIO.succeed(End)
          case other => ZIO.dieMessage(s"encountered invalid opcode $other")
        }

      def calculateNextPosition(position: Position, inst: Instruction): UIO[Position] =
        ZIO.succeed(position.skipN(inst.size))

      def executeInstruction(instruction: Instruction): ZIO[Any, Nothing, Unit] =
        instruction match {
          case Add(src1, src2, dest) => (memory.read(src1) zipWith memory.read(src2))(_ + _) >>= memory.write(dest)
          case Mul(src1, src2, dest) => (memory.read(src1) zipWith memory.read(src2))(_ * _) >>= memory.write(dest)
          case End                   => ZIO.succeed(())
        }

      override def interpreter(pos: Position): ZIO[Any, Nothing, List[Int]] =
        decodeInstruction(pos).flatMap {
          case End  => memory.asList
          case inst => executeInstruction(inst) *> calculateNextPosition(pos, inst) >>= interpreter
        }
    }
  }
}
