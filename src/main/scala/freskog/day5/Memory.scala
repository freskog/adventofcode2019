package freskog.day5

import zio.ZIO

trait Memory {
  val memory: Memory.Service[Any]
}

object Memory {

  trait Service[R] {
    def write(p: Position)(v: Int): ZIO[R, Nothing, Unit]
    def writeBool(p: Position)(b: Boolean): ZIO[R, Nothing, Unit]

    def read(p: Position): ZIO[R, Nothing, Int]
    def readBool(p: Position): ZIO[R, Nothing, Boolean]

    def region(p: Position, length: Int): ZIO[R, Nothing, Iterator[Int]]

    def dump: ZIO[R, Nothing, List[Int]]
  }

  trait Live extends Memory {

    val storage: Array[Int]

    override val memory: Service[Any] =
      new Service[Any] {
        override def write(p: Position)(v: Int): ZIO[Any, Nothing, Unit] =
          ZIO.effectTotal(storage(p.i) = v)

        def writeBool(p: Position)(b: Boolean): ZIO[Any, Nothing, Unit] =
          ZIO.effectTotal(storage(p.i) = if(b) 1 else 0)

        override def read(p: Position): ZIO[Any, Nothing, Int] =
          ZIO.effectTotal(storage(p.i))

        override def readBool(p: Position): ZIO[Any, Nothing, Boolean] =
          ZIO.effectTotal(storage(p.i)).map(_ != 0)

        override def region(p: Position, length: Int): ZIO[Any, Nothing, Iterator[Int]] =
          ZIO.effectTotal(storage.slice(p.i, p.i + length).iterator)

        override def dump: ZIO[Any, Nothing, List[Int]] =
          ZIO.effectTotal(storage.toList)

      }
  }
}
