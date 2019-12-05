package freskog.day5


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

    val stdIn:Ref[List[Int]]
    val stdOut:Ref[List[Int]]

    val memory:Memory.Service[Any]

    override val io: Service[Any] = new Service[Any] {
      override def read(p: Position): ZIO[Any, Nothing, Unit] =
        stdIn.modify(l => (l.head, l.tail)) >>= memory.write(p)

      override def write(v: Int): ZIO[Any, Nothing, Unit] =
        stdOut.update( v :: _ ).unit

      override def allWrittenValues: ZIO[Any, Nothing, List[Int]] =
        stdOut.get.map(_.reverse)
    }
  }
}
