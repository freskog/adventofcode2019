package freskog.day11

import freskog.day9.{ Computer, InstructionDecoder, Memory, Position }
import freskog.decodeCommaSeparatedAsMap
import zio._
import zio.console.Console

object SolutionDay11 extends App {

  def intCode(robotAI: Map[BigInt, BigInt], emptyMap: Ref[Map[Pos, List[Paint]]]): UIO[Computer] =
    for {
      posRef         <- Ref.make(Option.empty[Position])
      memRef         <- Ref.make(robotAI)
      relBaseInitial <- Ref.make(Position(0))
      curPos         <- Ref.make(Pos(0, 0))
      curMode        <- Ref.make[Mode](WaitingForPaint)
      direction      <- Ref.make[Direction](U)
    } yield
      new Computer.Live with Memory.Live with InstructionDecoder.Live with RobotIO {
        override val storage: Ref[Map[BigInt, BigInt]]   = memRef
        override val nextPosition: Ref[Option[Position]] = posRef
        override val relBase: Ref[Position]              = relBaseInitial
        override val visited: Ref[Map[Pos, List[Paint]]] = emptyMap
        override val currentPos: Ref[Pos]                = curPos
        override val currentMode: Ref[Mode]              = curMode
        override val facing: Ref[Direction]              = direction
      }

  def runProgram(program: Map[BigInt, BigInt], init: Ref[Map[Pos, List[Paint]]]): ZIO[Any, Nothing, List[BigInt]] =
    ZIO.accessM[Computer](_.computer.interpreter(Position(0))).provideM(intCode(program, init))

  def runAI(program: Map[BigInt, BigInt]): ZIO[Console, Nothing, Unit] =
    for {
      visited <- Ref.make( Map.empty[Pos,List[Paint]])
      _       <- runProgram(program, visited)
      r       <- visited.get
      _       <- console.putStrLn(s"painted ${r.size} positions")
    } yield ()

  def draw(map:Map[Pos,List[Paint]]): UIO[String] = ZIO.effectTotal {
    val minX = map.keys.minBy(_.x).x
    val minY = map.keys.minBy(_.y).y
    val maxX = map.keys.maxBy(_.x).x
    val maxY = map.keys.maxBy(_.y).y
    val width = (maxX - minX) + 1
    val height = maxY - minY + 1
    val array = Array.fill(height)(Array.fill[Paint](width)(Black))
    map.foreachEntry((pos, paints) => array(pos.y - minY)(pos.x - minX) = paints.head)
    array.map(_.map(_.render).mkString("")).reverse.mkString("\n")
  }


  def runAI2(program: Map[BigInt, BigInt]): ZIO[Console, Nothing, Unit] =
    for {
      visited <- Ref.make[Map[Pos,List[Paint]]](Map(Pos(0,0) -> List(White)))
      _       <- runProgram(program, visited)
      r       <- visited.get.flatMap(draw)
      _       <- console.putStrLn(r)
    } yield ()

  val partOne: ZIO[Console, Nothing, Unit] =
    decodeCommaSeparatedAsMap("freskog/day11/input-day11.txt").orDie.flatMap(runAI)

  val partTwo: ZIO[Console, Nothing, Unit] =
    decodeCommaSeparatedAsMap("freskog/day11/input-day11.txt").orDie.flatMap(runAI2)

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (partOne *> partTwo).as(0)
}
