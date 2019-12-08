package freskog.day7

import freskog.decodeCommaSeparatedAsArray
import zio.{ IO => _, _ }
import zio.console.Console
import zio.stream.ZStream

object SolutionDay7 extends App {

  def intCode(initial: Array[Int], in: Queue[Int], out: Queue[Int]): UIO[Computer] =
    for {
      outRef <- Ref.make(List.empty[Int])
      posRef <- Ref.make(Option.empty[Position])
    } yield
      new Computer.Live with Memory.Live with InstructionDecoder.Live with IO.Live {
        override val storage: Array[Int]                 = Array.copyOf(initial, initial.length)
        override val stdIn: Queue[Int]                   = in
        override val stdOut: Queue[Int]                  = out
        override val copyOfWrittenValues: Ref[List[Int]] = outRef
        override val nextPosition: Ref[Option[Position]] = posRef
      }

  def runProgram(program: Array[Int], in: Queue[Int], out: Queue[Int]): ZIO[Any, Nothing, List[Int]] =
    ZIO.accessM[Computer](_.computer.interpreter(Position(0))).provideM(intCode(program, in, out))

  def phases(start: Int, end: Int): ZStream[Any, Nothing, (Int, Int, Int, Int, Int)] =
    ZStream.fromIterator(ZIO.effectTotal((start to end).permutations.map(_.toList).map {
      case p1 :: p2 :: p3 :: p4 :: p5 :: Nil => (p1, p2, p3, p4, p5)
    }))

  def amplifierControllerSoftware(in: Queue[Int], out: Queue[Int]): ZIO[Any, Nothing, Fiber[Nothing, Int]] =
    decodeCommaSeparatedAsArray("freskog/day7/input-day7.txt").orDie >>= (runProgram(_, in, out).map(_.head).fork)

  def computeOutput(p1: Int, p2: Int, p3: Int, p4: Int, p5: Int): ZIO[Any, Nothing, ((Int, Int, Int, Int, Int), Int)] =
    for {
      inAmp1 <- Queue.bounded[Int](2)
      inAmp2 <- Queue.bounded[Int](1)
      inAmp3 <- Queue.bounded[Int](1)
      inAmp4 <- Queue.bounded[Int](1)
      inAmp5 <- Queue.bounded[Int](1)
      _      <- inAmp1.offer(p1) *> inAmp2.offer(p2) *> inAmp3.offer(p3) *> inAmp4.offer(p4) *> inAmp5.offer(p5)
      _      <- inAmp1.offer(0)
      _      <- amplifierControllerSoftware(inAmp1, inAmp2)
      _      <- amplifierControllerSoftware(inAmp2, inAmp3)
      _      <- amplifierControllerSoftware(inAmp3, inAmp4)
      _      <- amplifierControllerSoftware(inAmp4, inAmp5)
      amp5f  <- amplifierControllerSoftware(inAmp5, inAmp1)
      r      <- amp5f.join
    } yield (p1, p2, p3, p4, p5) -> r

  val partOne: ZIO[Console, Nothing, Unit] =
    phases(0, 4).mapM(ps => computeOutput(ps._1, ps._2, ps._3, ps._4, ps._5)).runCollect.map(_.maxBy(_._2)).flatMap {
      case (ps, output) => console.putStrLn(s"part one: $ps -> $output is the max")
    }

  val partTwo: ZIO[Console, Nothing, Unit] =
    phases(5, 9).mapM(ps => computeOutput(ps._1, ps._2, ps._3, ps._4, ps._5)).runCollect.map(_.maxBy(_._2)).flatMap {
      case (ps, output) => console.putStrLn(s"part two: $ps -> $output is the max")
    }

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (partOne *> partTwo).as(0)
}
