package freskog.day15

import freskog.day9.{ Computer, InstructionDecoder, Memory, Position }
import zio._
import zio.console.Console

object SolutionDay15 extends App {

  def intCode(program: Map[BigInt, BigInt]): ZIO[Any, Nothing, Computer with Droid] =
    for {
      posRef         <- Ref.make(Option.empty[Position])
      memRef         <- Ref.make(program)
      relBaseInitial <- Ref.make(Position(0))
      curPos         <- Ref.make(Pos(0, 0))
      worldRef       <- Ref.make(Map.empty[Pos, Tile])
      moveQ          <- Queue.unbounded[Movement]
      tileQ          <- Queue.unbounded[Tile]
    } yield
      new Computer.Live with Memory.Live with InstructionDecoder.Live with DroidIO with Droid.Live with Console.Live {
        override val storage: Ref[Map[BigInt, BigInt]]   = memRef
        override val nextPosition: Ref[Option[Position]] = posRef
        override val relBase: Ref[Position]              = relBaseInitial
        override val movement: Queue[Movement]           = moveQ
        override val status: Queue[Tile]                 = tileQ
        override val droidPos: Ref[Pos]                  = curPos
        override val world: Ref[Map[Pos, Tile]]          = worldRef
      }

  def shortestPath(program: Map[BigInt, BigInt]): ZIO[Any, Nothing, List[Movement]] =
    for {
      env <- intCode(program)
      _   <- ZIO.accessM[Computer](_.computer.interpreter(Position(0))).provide(env).fork
      r   <- ZIO.accessM[Droid](_.droid.shortestPathToGoal).provide(env)
    } yield r

  def timeToOxygenateMap(program: Map[BigInt, BigInt]): ZIO[Any, Nothing, Int] =
    for {
      env <- intCode(program)
      _   <- ZIO.accessM[Computer](_.computer.interpreter(Position(0))).provide(env).fork
      r   <- ZIO.accessM[Droid](_.droid.timeToOygenateMap).provide(env)
    } yield r


  val partOne: ZIO[Console, Nothing, Unit] =
    freskog
      .decodeCommaSeparatedAsMap("freskog/day15/input-day15.txt")
      .orDie
      .flatMap(shortestPath)
      .flatMap(l => console.putStrLn(s"${l.size} moves required"))

  val partTwo: ZIO[Console, Nothing, Unit] =
    freskog
      .decodeCommaSeparatedAsMap("freskog/day15/input-day15.txt")
      .orDie
      .flatMap(timeToOxygenateMap)
      .flatMap(m => console.putStrLn(s"It took $m minutes to oxygenate map"))


  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (partOne *> partTwo).as(0)
}
