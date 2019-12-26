package freskog.day17

import freskog.day9.{IO, Memory, Position}
import zio.{Queue, Ref, ZIO}
import zio.console.Console

trait AsciiIO extends IO {

  val memory:Memory.Service[Any]
  val console:Console.Service[Any]

  val allValues:Ref[List[BigInt]]
  val input:Queue[BigInt]

  override val io: IO.Service[Any] = new IO.Service[Any] {
    override def read(p: Position): ZIO[Any, Nothing, Unit] =
      input.take >>= memory.write(p)

    override def write(v: BigInt): ZIO[Any, Nothing, Unit] =
      allValues.update(v :: _).unit

    override def allWrittenValues: ZIO[Any, Nothing, List[BigInt]] =
      allValues.get.map(_.reverse)

  }
}
