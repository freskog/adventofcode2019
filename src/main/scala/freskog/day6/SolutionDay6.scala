package freskog.day6

import freskog.day6.SolutionDay6.Tree.{Leaf, Node}
import zio._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import zio.console.Console

object SolutionDay6 extends App {

  val orbitPattern = """(\w+)\)(\w+)""".r

  sealed trait Tree {

    def name:String

    def sumOfDepths(currentDepth: Int): Int = this match {
      case Node(_, children) => children.map(_.sumOfDepths(currentDepth + 1)).foldLeft(currentDepth)(_ + _)
      case Leaf(_)           => currentDepth
    }

    def nodes: List[String] =
      this match {
        case Leaf(name)           => List(name)
        case Node(name, children) => name :: children.flatMap(_.nodes)
      }

    def edges: List[UnDiEdge[String]] =
      this match {
        case Node(name,children) => children.foldLeft(children.map(name ~ _.name))(_ ::: _.edges)
        case Leaf(_) => Nil
      }

    lazy val graph: Graph[String, UnDiEdge] =
      Graph.from(nodes, edges)

    def distanceBetween(from:String, to:String):Int =
      graph.get(from).shortestPathTo(graph.get(to)).fold(0)(_.length)
  }

  object Tree {
    case class Node(name: String, children: List[Tree]) extends Tree
    case class Leaf(name: String)                       extends Tree

    def from(m: Map[String, List[String]], startNode: String): Tree =
      if (m.contains(startNode)) Node(startNode, m(startNode).map(Tree.from(m, _)))
      else Leaf(startNode)
  }

  def mergeIntoOrbits(m: Map[String, List[String]], orbit: (String, String)): Map[String, List[String]] =
    m.updated(orbit._1, orbit._2 :: m.getOrElse(orbit._1, Nil))

  val rawOrbits: ZIO[Any, Nothing, Map[String, List[String]]] =
    freskog
      .decodeLines("freskog/day6/input-day6.txt")
      .map { case orbitPattern(orbited, orbiter) => (orbited, orbiter) }
      .fold(Map.empty[String, List[String]])(mergeIntoOrbits)
      .orDie

  val partOne: ZIO[Console, Nothing, Unit] =
    rawOrbits
      .map(Tree.from(_, "COM").sumOfDepths(0))
      .flatMap(n => console.putStrLn(s"code is $n"))


  val partTwo: ZIO[Console, Nothing, Unit] =
    rawOrbits
      .map(Tree.from(_, "COM").distanceBetween("YOU","SAN")-2)
      .flatMap(n => console.putStrLn(s"distance is $n"))


  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (partOne *> partTwo).as(0)

}
