package freskog.day19

import freskog.day9.{IO, Memory, Position}
import zio._
import zio.console._

trait DroneIO extends IO {

  val memory:Memory.Service[Any]
  val console:Console.Service[Any]

  val input:Queue[BigInt]
  val output:Ref[List[BigInt]]

  override val io: IO.Service[Any] = new IO.Service[Any] {
    override def read(p: Position): ZIO[Any, Nothing, Unit] =
      input.take >>= memory.write(p)

    override def write(v: BigInt): ZIO[Any, Nothing, Unit] =
      output.update(v :: _).unit

    override def allWrittenValues: ZIO[Any, Nothing, List[BigInt]] =
      output.get
  }
}
