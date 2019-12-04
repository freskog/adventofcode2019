package freskog.day3

import freskog.decodeCommaSeparatedOnMultipleLines
import zio._
import zio.console.Console

import scala.util.Try

object SolutionDay3 extends App {

  sealed abstract class Direction
  case object L extends Direction
  case object R extends Direction
  case object U extends Direction
  case object D extends Direction

  object Direction {
    def from(c: String): Either[String, Direction] = c match {
      case "U" => Right(U)
      case "D" => Right(D)
      case "L" => Right(L)
      case "R" => Right(R)
      case _   => Left("Invalid Direction")
    }
  }

  case class Point(x: Int, y: Int) {
    def relativeTo(d: Direction, unit: Int): Point = d match {
      case U => Point(x, y + unit)
      case D => Point(x, y - unit)
      case R => Point(x + unit, y)
      case L => Point(x - unit, y)
    }
  }

  case class Line(origin: Point, d: Direction, length: Int) {
    def asPoints: List[Point] =
      Iterator
        .unfold(0) {
          case `length` => None
          case n        => Some(origin.relativeTo(d, n), n + 1)
        }
        .toList

    def destination: Point =
      origin.relativeTo(d, length)
  }

  def length(n: String): Either[String, Int] =
    Try(n.toInt).toEither.fold[Either[String, Int]](e => Left(e.getMessage), Right(_))

  def distanceToOrigin(p: Point): Int = Math.abs(p.x) + Math.abs(p.y)

  def expandLinesToPoints(input: List[Line]): List[Point] =
    input.foldRight(List.empty[Point]) {
      (line, points) => if(points.isEmpty) line.asPoints ::: List(line.destination) else line.asPoints ::: points
    }

  def parseLine(input: List[String]): Either[String, List[Line]] = {
    val directionPattern = """(\w)(\d+)""".r
    input.foldLeft[Either[String, List[Line]]](Right(Nil)) {
      case (parsed, directionPattern(d, n)) =>
        for {
          prevParsed     <- parsed
          validDirection <- Direction.from(d)
          validLength    <- length(n)
          origin         = prevParsed.headOption.map(_.destination).getOrElse(Point(0, 0))
        } yield Line(origin, validDirection, validLength) :: prevParsed
    }.map(_.reverse)
  }

  def generateAllPointsFrom(input: List[List[String]]): ZIO[Any, String, List[List[Point]]] =
    ZIO.foreach(input)(l => ZIO.fromEither(parseLine(l).map(expandLinesToPoints)))

  def calculateIntersectionsBetween(input: List[List[String]]): ZIO[Any, String, List[Point]] =
    generateAllPointsFrom(input).map(flattenToIntersectingPoints)

  def flattenToIntersectingPoints: List[List[Point]] => List[Point] =
    _.reduceLeft((l1, l2) => (l1.toSet.intersect(l2.toSet) - Point(0, 0)).toList)

  def calculateClosestIntersection(input: List[List[String]]): ZIO[Any, String, Point] =
    calculateIntersectionsBetween(input).map(_.minBy(distanceToOrigin))

  def calculateStepsToEachIntersection(wire: List[Point], intersections: List[Point]): Map[Point, Int] =
    wire
      .foldLeft((0, Map.empty[Point, Int])) {
        case ((steps, acc), p) if acc.contains(p)           => (steps, acc)
        case ((steps, acc), p) if intersections.contains(p) => (steps + 1, acc.updated(p, steps))
        case ((steps, acc), _)                              => (steps + 1, acc)
      }
      ._2

  def mergePointAndSteps(m1: Map[Point, Int], m2: Map[Point, Int]): Map[Point, Int] =
    m1.foldLeft(m2) { case (acc, (p, s)) => acc.updated(p, acc(p) + s) }

  def calculateFewestSteps(input: List[List[String]]): ZIO[Any, String, (Point, Int)] =
    generateAllPointsFrom(input).map { allPoints =>
      val intesections = flattenToIntersectingPoints(allPoints)
      allPoints.map(calculateStepsToEachIntersection(_, intesections)).reduce(mergePointAndSteps)
    }.map(_.minBy(_._2))

  val input: ZIO[Any, Nothing, List[List[String]]] =
    decodeCommaSeparatedOnMultipleLines("freskog/day3/input-day3.txt").orDie

  val partOne: ZIO[Console, Nothing, Int] =
    (input >>= calculateClosestIntersection)
      .foldM(
        err => console.putStrLn(err),
        p => console.putStrLn(s"$p is the closest intersection (dist = ${distanceToOrigin(p)})")
      )
      .as(0)

  val partTwo: ZIO[Console, Nothing, Int] =
    (input >>= calculateFewestSteps)
      .foldM(
        err => console.putStrLn(err),
        p => console.putStrLn(s"${p._1} is has the fewest steps intersection, steps =  ${p._2}")
      )
      .as(0)

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    partOne *> partTwo
}
