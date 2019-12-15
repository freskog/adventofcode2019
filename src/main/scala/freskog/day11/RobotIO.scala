package freskog.day11

import freskog.day9.{ IO, Memory, Position }
import zio.{ Ref, UIO, ZIO }

trait RobotIO extends IO {

  val visited: Ref[Map[Pos, List[Paint]]]
  val currentPos: Ref[Pos]
  val facing: Ref[Direction]
  val currentMode: Ref[Mode]

  val memory: Memory.Service[Any]

  def getCurrentPaint: ZIO[Any, Nothing, Paint] =
    for {
      pos <- currentPos.get
      map <- visited.get
    } yield if (map.contains(pos)) map(pos).head else Black

  def updatePos(v: BigInt): UIO[Unit] =
    for {
      direction    <- facing.get
      newDirection <- currentPos.modify(pos => if (Turn.from(v) == Right) pos.right(direction) else pos.left(direction))
      _            <- facing.set(newDirection)
    } yield ()

  def paintCurrentPos(v: BigInt): ZIO[Any, Nothing, Unit] =
    currentPos.get.flatMap(
      pos =>
        visited
          .update(
            map => map.updated(pos, Paint.from(v) :: map.getOrElse(pos, Nil))
          ).unit
    )

  def nextMode: UIO[Mode] =
    currentMode.update(_.next) // <* currentMode.get.flatMap(m => ZIO.effectTotal(println(s"nextMode => m = $m (was ${m.next})")))

  def processWrite(v: BigInt): ZIO[Any, Nothing, Unit] =
    for {
      mode <- currentMode.get
      _    <- ZIO.when(mode == WaitingForTurn)(updatePos(v))
      _    <- ZIO.when(mode == WaitingForPaint)(paintCurrentPos(v))
      _    <- nextMode
    } yield ()

  override val io: IO.Service[Any] = new IO.Service[Any] {
    override def read(p: Position): ZIO[Any, Nothing, Unit] =
      getCurrentPaint.map(_.asBigInt) >>= memory.write(p)

    override def write(v: BigInt): ZIO[Any, Nothing, Unit] =
      processWrite(v)

    override def allWrittenValues: ZIO[Any, Nothing, List[BigInt]] =
      ZIO.succeed(Nil)
  }
}

sealed abstract class Direction
case object L extends Direction
case object R extends Direction
case object U extends Direction
case object D extends Direction

case class Pos(x: Int, y: Int) {
  def right(facing: Direction): (Direction, Pos) =
    facing match {
      case L => (U, Pos(x, y + 1))
      case R => (D, Pos(x, y - 1))
      case U => (R, Pos(x + 1, y))
      case D => (L, Pos(x - 1, y))
    }
  def left(facing: Direction): (Direction, Pos) =
    facing match {
      case L => (D, Pos(x, y - 1))
      case R => (U, Pos(x, y + 1))
      case U => (L, Pos(x - 1, y))
      case D => (R, Pos(x + 1, y))
    }

}
sealed abstract class Paint {
  def asBigInt: BigInt = this match {
    case White => BigInt(1)
    case Black => BigInt(0)
  }
  def render:String = this match {
    case Black => " "
    case White => "\u2588"
  }
}
object Paint {
  def from(int: BigInt): Paint =
    if (int == BigInt(1)) White else Black
}
case object White extends Paint
case object Black extends Paint
sealed abstract class Turn {
  def asBigInt: BigInt = this match {
    case Left  => BigInt(0)
    case Right => BigInt(1)
  }
}
object Turn {
  def from(int: BigInt): Turn =
    if (int == BigInt(1)) Right else Left
}
case object Left  extends Turn
case object Right extends Turn

sealed abstract class Mode {
  def next: Mode = this match {
    case WaitingForPaint => WaitingForTurn
    case WaitingForTurn  => WaitingForPaint
  }
}
object Mode
case object WaitingForPaint extends Mode
case object WaitingForTurn  extends Mode
