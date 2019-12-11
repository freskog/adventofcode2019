package freskog.day10

import zio._
import zio.console.Console

import scala.collection.SortedSet

object SolutionDay10 extends App {

  case class Point(x:Int, y:Int) {
    def translateTo(newOrigin:Point):Point =
      Point(x - newOrigin.x, y - newOrigin.y)

    override def toString:String = s"Point($x,${math.abs(y)})"
  }

  object Point {
    def from(input:String):List[Point] =
      input.foldLeft((0,0,List.empty[Point])) {
        case ((x,y,acc),'.') => (x+1,y,acc)
        case ((x,y,acc),'#') => (x+1,y,Point(x,y) :: acc)
        case ((_,y,acc),'\n') => (0,y-1,acc)
      }._3
  }

  case class Line(angle:Double, magnitude:Double, from:Point, to:Point)

  object Line {

    implicit val angleMagnitudeOrder: Ordering[Line] =
      Ordering.fromLessThan[Line] {
        case (Line(a1,m1,_,_),Line(a2,m2,_,_)) => if(a1 == a2) m1 < m2 else a1 < a2
      }

    def between(origin:Point, to:Point):Line = {
      val Point(x,y) =  to.translateTo(origin)
      val angle = math.toDegrees(math.atan2(x,y))
      val magnitude = math.sqrt((x*x) + (y*y))
      Line(if(angle < 0) 360.0 + angle else angle, magnitude, origin, to)
    }
  }

  def linesFrom(origin:Point, allPoints:List[Point]): Map[Double, SortedSet[Line]] =
    allPoints.filter(_ != origin).map(p => Line.between(origin, p)).groupBy(_.angle).map {
      case (angle, lines) => (angle, SortedSet(lines:_*))
    }

  def linesForAll(points:List[Point]): Map[Point, Map[Double, SortedSet[Line]]] =
    points.foldLeft(Map.empty[Point, Map[Double, SortedSet[Line]]])((m, origin) =>
      m.updated(origin, linesFrom(origin, points))
    )

  def determineNewCenter(points:List[Point]): (Point, Int) =
    linesForAll(points).maxBy(_._2.size) match {
      case (p, m) => (p, m.size)
    }

  def determineEliminationOrder(allPoints:List[Point]):Map[Int,Point] = {
    val center = determineNewCenter(allPoints)._1
    val lines = linesFrom(center, allPoints)
    val angles = lines.keys.toList.sorted
    def peel(idx:Int, acc:Map[Int, Point], rem:Map[Double, SortedSet[Line]]):Map[Int, Point] =
      if(rem.isEmpty) acc
      else (peel _).tupled(
        angles.foldLeft((idx,acc,rem)) {
          case ((n, acc, rem), angle) if rem.contains(angle) =>
            (n+1, acc.updated(n, lines(angle).head.to), if(rem(angle).size == 1) rem - angle else rem.updated(angle, rem(angle).tail))
          case ((n, acc, rem), _) => (n, acc, rem)
        })
    peel(1,Map.empty,lines)
  }

  val partOne: ZIO[Console, Nothing, Unit] =
    freskog.decodeAsRawString("freskog/day10/input-day10.txt").orDie
      .map(Point.from _ andThen determineNewCenter).flatMap(
      r => console.putStrLn(s"part one: $r")
    )

  val partTwo: ZIO[Console, Nothing, Unit] =
    freskog.decodeAsRawString("freskog/day10/input-day10.txt").orDie
      .map(Point.from _ andThen determineEliminationOrder).flatMap(
      r => console.putStrLn(s"part two: ${r(200)}")
    )

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (partOne *> partTwo).as(0)

}
