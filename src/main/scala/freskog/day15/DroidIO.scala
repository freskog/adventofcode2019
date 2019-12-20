package freskog.day15

import freskog.day9.{IO, Memory, Position}
import zio.{Queue, ZIO}

trait DroidIO extends IO {

  val memory: Memory.Service[Any]
  val movement: Queue[Movement]
  val status: Queue[Tile]

  override val io: IO.Service[Any] = new IO.Service[Any] {
    override def read(p: Position): ZIO[Any, Nothing, Unit] =
      movement.take.map(_.asBigInt) >>= memory.write(p)

    override def write(v: BigInt): ZIO[Any, Nothing, Unit] =
      status.offer(Tile.from(v)).unit

    override def allWrittenValues: ZIO[Any, Nothing, List[BigInt]] =
      ZIO.succeed(Nil)
  }
}
