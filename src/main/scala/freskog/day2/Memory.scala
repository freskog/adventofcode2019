package freskog.day2

import zio.ZIO

trait Memory {
  val memory:Memory.Service[Any]
}

object Memory {

  trait Service[R] {
    def write(p: Position)(v: Int): ZIO[R, Nothing, Unit]

    def read(p: Position): ZIO[R, Nothing, Int]

    def asList: ZIO[R, Nothing, List[Int]]
  }

  trait Live extends Memory {

    val input: Array[Int]

    override val memory: Service[Any] = new Service[Any] {
      override def write(p: Position)(v: Int): ZIO[Any, Nothing, Unit] =
        ZIO.effectTotal(input(p.i) = v)

      override def read(p: Position): ZIO[Any, Nothing, Int] =
        ZIO.effectTotal(input(p.i))

      override def asList: ZIO[Any, Nothing, List[Int]] =
        ZIO.effectTotal(input.toList)
    }
  }
}