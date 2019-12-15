package freskog.day9

import zio.{ Ref, ZIO }

trait Memory {
  val memory: Memory.Service[Any]
}

object Memory {

  trait Service[R] {
    def write(p: Position)(v: BigInt): ZIO[R, Nothing, Unit]

    def writeBool(p: Position)(b: Boolean): ZIO[R, Nothing, Unit] =
      if (b) write(p)(1) else write(p)(0)

    def read(p: Position): ZIO[R, Nothing, BigInt]

    def readBool(p: Position): ZIO[R, Nothing, Boolean] =
      read(p).map(_ != 0)

    def region(p: Position, length: Int): ZIO[R, Nothing, List[BigInt]]

    def dump: ZIO[R, Nothing, Map[BigInt, BigInt]]
  }

  trait Live extends Memory {

    val storage: Ref[Map[BigInt, BigInt]]

    override val memory: Service[Any] =
      new Service[Any] {
        def validate(p: Position): ZIO[Any, Nothing, Position] =
          if (p.i < 0) ZIO.dieMessage(s"Attempted to use negative position $p") else ZIO.succeed(p)

        override def write(p: Position)(v: BigInt): ZIO[Any, Nothing, Unit] =
          validate(p) *> storage.update(_.updated(p.i, v)).unit

        override def read(p: Position): ZIO[Any, Nothing, BigInt] =
          validate(p) *> storage.get.map(_.getOrElse(p.i, BigInt(0)))

        override def region(p: Position, length: Int): ZIO[Any, Nothing, List[BigInt]] =
          validate(p) *> ZIO.foreach(p.i to p.i + length)(i => read(Position(i)))

        override def dump: ZIO[Any, Nothing, Map[BigInt, BigInt]] =
          storage.get

      }
  }
}
