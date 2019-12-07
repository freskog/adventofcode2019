package freskog.day7


import zio._

trait IO {
  val io:IO.Service[Any]
}

object IO {
  trait Service[R] {
    def read(p:Position):ZIO[Any, Nothing, Unit]
    def write(v:Int):ZIO[Any,Nothing,Unit]
    def allWrittenValues:ZIO[Any, Nothing, List[Int]]
  }

  trait Live extends IO {

    val stdIn:Queue[Int]
    val stdOut:Queue[Int]
    val copyOfWrittenValues:Ref[List[Int]]

    val memory:Memory.Service[Any]

    override val io: Service[Any] = new Service[Any] {
      override def read(p: Position): ZIO[Any, Nothing, Unit] =
        stdIn.take >>= memory.write(p)

      override def write(v: Int): ZIO[Any, Nothing, Unit] =
        stdOut.offer(v).unit <* copyOfWrittenValues.update(v :: _)

      override def allWrittenValues: ZIO[Any, Nothing, List[Int]] =
        copyOfWrittenValues.get
    }
  }
}
