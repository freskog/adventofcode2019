package freskog.day6

import cats.Functor
import zio._
import higherkindness.droste._
import zio.console.Console

object SolutionDay6Alt extends App {

  final case class NodeF[A](name: String, lst: List[A])

  object NodeF {
    implicit val nodeFunc: Functor[NodeF] = new Functor[NodeF] {
      override def map[A, B](fa: NodeF[A])(f: A => B): NodeF[B] = NodeF(fa.name, fa.lst.map(f))
    }
  }

  def buildOrbits(orbits: Map[String, List[String]]): Coalgebra[NodeF, String] =
    Coalgebra(name => if (orbits.contains(name)) NodeF(name, orbits(name)) else NodeF(name, Nil))

  val allOrbits: Algebra[NodeF, (Int, Int)] =
    Algebra {
      case NodeF(_, lst) =>
        val children = lst.map(_._1 + 1).sum
        val orbits   = lst.map(_._2).sum
        (children, children + orbits)
    }

  def calculateAllOrbits(orbits: Map[String, List[String]]): String => (Int, Int) =
    scheme.hylo(allOrbits, buildOrbits(orbits))

  case class Accum(dist: Int, seenSa: Boolean, seenMe: Boolean)

  val distFromSantaToMe: Algebra[NodeF, Accum] =
    Algebra {
      case NodeF("SAN", lst) => if (lst.exists(_.seenMe)) lst(lst.indexWhere(_.seenMe)) else Accum(-1, true, false)
      case NodeF("YOU", lst) => if (lst.exists(_.seenSa)) lst(lst.indexWhere(_.seenSa)) else Accum(-1, false, true)
      case NodeF(_, lst) =>
        (lst.indexWhere(_.seenSa), lst.indexWhere(_.seenMe)) match {
          case (-1, -1)         => Accum(0, false, false)
          case (-1, n)          => Accum(1 + lst(n).dist, false, true)
          case (n, -1)          => Accum(1 + lst(n).dist, true, false)
          case (n, m) if n == m => Accum(0 + lst(n).dist, true, true)
          case (n, m)           => Accum(2 + lst(n).dist + lst(m).dist, true, true)
        }

    }

  def calculateDistBetweenSantaAndMe(orbits: Map[String, List[String]]): String => Accum =
    scheme.hylo(distFromSantaToMe, buildOrbits(orbits))

  def mergeIntoOrbits(m: Map[String, List[String]], orbit: (String, String)): Map[String, List[String]] =
    m.updated(orbit._1, orbit._2 :: m.getOrElse(orbit._1, Nil))

  val orbitPattern = """(\w+)\)(\w+)""".r

  val rawOrbits: ZIO[Any, Nothing, Map[String, List[String]]] =
    freskog
      .decodeLines("freskog/day6/input-day6.txt")
      .map { case orbitPattern(orbited, orbiter) => (orbited, orbiter) }
      .fold(Map.empty[String, List[String]])(mergeIntoOrbits)
      .orDie

  val partOne: ZIO[Console, Nothing, Unit] =
    rawOrbits.map(calculateAllOrbits(_)("COM")._2) >>=
      (r => console.putStrLn(s"result is $r"))

  val partTwo: ZIO[Console, Nothing, Unit] =
    rawOrbits.map(calculateDistBetweenSantaAndMe(_)("COM").dist).flatMap { r =>
      console.putStrLn(s"dist between me and santa is $r")
    }

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (partOne *> partTwo).as(0)
}
