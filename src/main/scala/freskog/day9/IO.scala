package freskog.day9

import zio._

trait IO {
  val io: IO.Service[Any]
}

object IO {
  trait Service[R] {
    def read(p: Position): ZIO[Any, Nothing, Unit]
    def write(v: BigInt): ZIO[Any, Nothing, Unit]
    def allWrittenValues: ZIO[Any, Nothing, List[BigInt]]
  }

  trait Live extends IO {

    val stdIn: Queue[BigInt]
    val stdOut: Queue[BigInt]
    val copyOfWrittenValues: Ref[List[BigInt]]

    val memory: Memory.Service[Any]

    override val io: Service[Any] = new Service[Any] {
      override def read(p: Position): ZIO[Any, Nothing, Unit] =
        stdIn.take >>= memory.write(p)

      override def write(v: BigInt): ZIO[Any, Nothing, Unit] =
        stdOut.offer(v).unit <* copyOfWrittenValues.update(v :: _)

      override def allWrittenValues: ZIO[Any, Nothing, List[BigInt]] =
        copyOfWrittenValues.get
    }
  }
}
