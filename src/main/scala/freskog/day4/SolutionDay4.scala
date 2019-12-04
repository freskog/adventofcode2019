package freskog.day4

import zio._
import zio.console.Console
import fastparse._, NoWhitespace._

object SolutionDay4 extends App {

  def number[_: P]: P[Int]                = P(CharPred(_.isDigit).!.map(_.toInt))
  def equalTo[_: P](n: Int): P[Int]       = P(number.filter(_ == n))
  def greaterThan[_: P](n: Int): P[Int]   = P(number.filter(_ > n))
  def greaterOrEqTo[_: P](n: Int): P[Int] = P(number.filter(_ >= n))

  def digitRepeatedExactly2[_: P](n: Int): P[Int] =
    P(greaterThan(n).flatMapX(n => equalTo(n) ~ &(!equalTo(n) | End)))

  def digitRepeatedAtLeast2[_: P](n: Int): P[Int] =
    P(greaterThan(n).flatMapX(n => equalTo(n)))

  def zeroOrMoreNonDecreasing[_: P](n: Int): P[Int] =
    P(greaterOrEqTo(n).flatMapX(zeroOrMoreNonDecreasing) | greaterOrEqTo(n) ~ End | End ~ Pass(n))

  def partOneParser[_: P](n: Int): P[Int] =
    P(greaterOrEqTo(n).flatMapX(partOneParser) | digitRepeatedAtLeast2(n).flatMapX(zeroOrMoreNonDecreasing))

  def partTwoParser[_: P](n: Int): P[Int] =
    P(greaterOrEqTo(n).flatMapX(partTwoParser) | digitRepeatedExactly2(n).flatMapX(zeroOrMoreNonDecreasing))
  
  val partOne: ZIO[Console, Nothing, Unit] =
    console
      .putStrLn(s"""${Iterator
        .from(153517)
        .take(630395 - 153517)
        .map(_.toString)
        .count(n => parse(n, partOneParser(0)(_)).isSuccess)} valid numbers in range""")
      .as(0)

  val partTwo: ZIO[Console, Nothing, Int] =
    console
      .putStrLn(s"""${Iterator
        .from(153517)
        .take(630395 - 153517)
        .map(_.toString)
        .count(n => parse(n, partTwoParser(0)(_)).isSuccess)} valid numbers in range""")
      .as(0)

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    partOne *> partTwo
}
