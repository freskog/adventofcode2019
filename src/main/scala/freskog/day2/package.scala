package freskog
import zio.ZIO

package object day2 extends Computer.Service[Computer] with Memory.Service[Memory] {
  override def interpreter(pos: Position): ZIO[Computer, Nothing, List[Int]] =
    ZIO.accessM[Computer](_.computer.interpreter(pos))

  override def write(p: Position)(v: Int): ZIO[Memory, Nothing, Unit] =
    ZIO.accessM[Memory](_.memory.write(p)(v))

  override def read(p: Position): ZIO[Memory, Nothing, Int] =
    ZIO.accessM[Memory](_.memory.read(p))

  override def dump: ZIO[Memory, Nothing, List[Int]] =
    ZIO.accessM[Memory](_.memory.dump)
}
