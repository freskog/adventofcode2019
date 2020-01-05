package freskog.day21

import freskog.day17.SolutionDay17.intCode
import freskog.day9.{Computer, Position}
import zio._
import zio.console.Console

object SolutionDay21 extends App {

  /*
    jumpIf (!A || !B || !C) && D
   */

  val instructionsPt1: String =
    """NOT A J
      |NOT B T
      |OR T J
      |NOT C T
      |OR T J
      |AND D J
      |WALK
      |""".stripMargin.mkString("")

  /*
    jumpIf (!A || !B || !C) && (D && (E || H)
   */
  val instructionsPt2: String =
    """NOT A J
      |NOT B T
      |OR T J
      |NOT C T
      |OR T J
      |AND D J
      |NOT H T
      |NOT T T
      |OR E T
      |AND T J
      |RUN
      |""".stripMargin

  def runProgram(program: Map[BigInt, BigInt], input: String): ZIO[Any, Nothing, List[BigInt]] =
    ZIO.accessM[Computer](_.computer.interpreter(Position(0))).provideM(intCode(program, input))

  def asString(input: List[BigInt]): String =
    input.map(_.toInt.toChar).mkString("")

  val program: URIO[Any, Map[BigInt, BigInt]] =
    freskog.decodeCommaSeparatedAsMap("freskog/day21/input-day21.txt").orDie

  val part1: ZIO[Console, Nothing, Unit] =
    program.flatMap(runProgram(_, instructionsPt1)).flatMap(l => console.putStrLn(s"part1 damages: ${l.last}"))

  val part2: ZIO[Console, Nothing, Unit] =
    program.flatMap(runProgram(_, instructionsPt2)).flatMap(l => console.putStrLn(s"part2 damages: ${l.last}"))

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (part1 *> part2).as(0)
}
