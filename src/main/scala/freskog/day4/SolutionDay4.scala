package freskog.day4

import zio._
import zio.console.Console

object SolutionDay4 extends App {

  val pairPattern = """(\d)(\d)""".r

  def nonDecreasing(numeric: String): Boolean =
    numeric.toSeq.sliding(2).map(_.toString).forall {
      case pairPattern(d1, d2) => d2.toInt >= d1.toInt
    }

  def negate(bool: Boolean): Boolean = !bool

  def hasAtLeastOneDouble(numeric: String): Boolean =
    negate(numeric.toSeq.sliding(2).map(_.toString()).forall {
      case pairPattern(d1, d2) => d1 != d2
    })

  def inRange(from: Int, to: Int): Iterator[String] =
    Iterator
      .from(from)
      .take(to - from)
      .map(_.toString)
      .filter(
        numeric => nonDecreasing(numeric) && hasAtLeastOneDouble(numeric)
      )

  val partOne: ZIO[Console, Nothing, Int] =
    ZIO
      .succeed(inRange(153517, 630395).size)
      .flatMap(
        n => console.putStrLn(s"$n digits in range 153517, 630395")
      )
      .as(0)

  import fastparse._, NoWhitespace._

  def number[_: P]: P[Int]                = P(CharPred(_.isDigit).!.map(_.toInt))
  def equalTo[_: P](n: Int): P[Int]       = P(number.filter(_ == n))
  def greaterThan[_: P](n: Int): P[Int]   = P(number.filter(_ > n))
  def greaterOrEqTo[_: P](n: Int): P[Int] = number.filter(_ >= n)

  def aPairGreaterThan[_: P](n: Int): P[Int] =
    P(greaterThan(n).flatMapX(n => equalTo(n) ~ &(!equalTo(n) | End)))

  def nonDecreasingSeq[_: P](n: Int): P[Int] =
    P(greaterOrEqTo(n).flatMapX(nonDecreasingSeq) | greaterOrEqTo(n) ~ End | End ~ Pass(n))

  def onlyStrictPairs[_: P](prev: Int): P[Int] =
    P(greaterOrEqTo(prev).flatMapX(onlyStrictPairs) | aPairGreaterThan(prev).flatMapX(nonDecreasingSeq))

  val partTwo: ZIO[Console, Nothing, Int] =
    console.putStrLn(s"""${Iterator
      .from(153517)
      .take(630395 - 153517)
      .map(_.toString)
      .count(n => parse(n, onlyStrictPairs(0)(_)).isSuccess)} valid numbers in range""").as(0)

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    partOne *> partTwo
}
