package freskog.day13

import freskog.day13.ArcadeIO.{Ball, Block, Empty, HorPaddle, Pos, Screen, Tile, Wall}
import freskog.day9.{IO, Memory, Position}
import zio.console.Console
import zio.{Ref, UIO, ZIO}

import scala.math.BigInt

trait ArcadeIO extends IO {

  val console: Console.Service[Any]

  val memory:Memory.Service[Any]

  val partialTile: Ref[List[BigInt]]
  val screen: Ref[Screen]
  val score: Ref[BigInt]

  def extractTile(b: BigInt): Tile = b.toInt match {
    case 0 => Empty
    case 1 => Wall
    case 2 => Block
    case 3 => HorPaddle
    case 4 => Ball
  }

  def extractPos(x: Int, y: Int): Pos =
    Pos(x.toInt, y.toInt)

  def updateScreen(tile: Tile, pos: Pos): UIO[Screen] =
    screen.update(s => s.copy(tiles = s.tiles.updated(pos, tile)))

  def updateScore(n:BigInt): UIO[Unit] =
    score.set(n)

  def processWrite(input: BigInt): UIO[Unit] =
    partialTile.get.map(_.map(_.toInt)).flatMap {
      case 0 :: -1 :: Nil =>partialTile.set(Nil) *> updateScore(input)
      case y :: x :: Nil => partialTile.set(Nil) *> updateScreen(extractTile(input), extractPos(x, y)).unit
      case _             => partialTile.update(input :: _).unit
    }

  def processRead: ZIO[Any, Nothing, BigInt] =
    for {
      current <- screen.get
      ballPos = current.getBallPos.x
      paddlePos = current.getPaddlePos.x
    } yield if(ballPos < paddlePos) BigInt(-1) else if(ballPos > paddlePos) BigInt(1) else BigInt(0)

  override val io: IO.Service[Any] = new IO.Service[Any] {
    override def read(p: Position): ZIO[Any, Nothing, Unit] =
      processRead >>= memory.write(p)

    override def write(v: BigInt): ZIO[Any, Nothing, Unit] =
      processWrite(v)

    override def allWrittenValues: ZIO[Any, Nothing, List[BigInt]] =
      score.get.map(List(_))
  }
}

object ArcadeIO {
  case class Pos(x: Int, y: Int)
  sealed abstract class Tile {
    def isBlock: Boolean = this match {
      case Block => true
      case _     => false
    }
    def isWall: Boolean = this match {
      case Wall => true
      case _    => false
    }
    def isBall: Boolean = this match {
      case Ball => true
      case _ => false
    }

    def render: String = this match {
      case Empty     => " "
      case Wall      => "\u2588"
      case Block     => "#"
      case HorPaddle => "-"
      case Ball      => "O"
    }
  }
  case object Empty     extends Tile
  case object Wall      extends Tile
  case object Block     extends Tile
  case object HorPaddle extends Tile
  case object Ball      extends Tile

  case class Screen(tiles: Map[Pos, Tile]) {
    def render: String =
      (tiles.keys.maxBy(_.y).y to 0 by -1).foldLeft(List.empty[String]) {
        case (acc, y) => (0 to tiles.keys.maxBy(_.x).x).foldLeft("")((s,x) => s ++ tiles(Pos(x,y)).render) :: acc
      }.mkString("\n")

    def getBallPos:Pos = tiles.collectFirst { case (pos, Ball) => pos }.get
    def getPaddlePos: Pos = tiles.collectFirst { case (pos, HorPaddle) => pos }.get
  }
}
