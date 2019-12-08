package freskog.day8

import zio._
import fastparse._, NoWhitespace._
import zio.console.Console

object SolutionDay8 extends App {

  final case class Pixel(n:Int) extends AnyVal {
    override def toString: String = if(n == 1) "*" else " "
  }

  final case class Layer(data:Vector[Vector[Pixel]]) {
    def count(p:Pixel):Int = data.map(_.count(_ == p)).sum

    override def toString:String =
      data.map(_.map(_.toString).mkString("")).mkString("\n")
  }

  def merge(top:Layer, bottom:Layer):Layer =
    Layer(bottom.data.zip(top.data).map((mergeRow _).tupled))

  def mergeRow(bottom:Vector[Pixel], top:Vector[Pixel]):Vector[Pixel] =
    (bottom.zip(top)).map {
      case (p, Pixel(2)) => p
      case (_, p) => p
    }

  def digit[_ : P]: P[Int] = P( CharPred(_.isDigit).!.map(_.toInt) )
  def pixel[_ : P]: P[Pixel] = P( digit.map(Pixel) )

  def row[_ : P](width:Int):P[Vector[Pixel]] =
    P( pixel.repX(exactly = width).map(_.toVector) )

  def layer[_ : P](width: Int, height: Int): P[Layer] =
    P( row(width).repX( exactly = height).map(_.toVector).map(Layer))

  def allLayers[_ : P](width: Int, height:Int):P[List[Layer]] =
    Start ~ P( layer(width, height).repX.map(_.toList) ) ~ End

  val inputAsString: ZIO[Any, Nothing, String] =
    freskog.decodeLines("freskog/day8/input-day8.txt").fold("")(_ ++ _).orDie

  val layers: ZIO[Console, Nothing, List[Layer]] =
    inputAsString.flatMap(parse(_, allLayers(25,6)(_)).fold(
      (_, at, x) => ZIO.dieMessage(s"error parsing layers at $at ${x.trace(true).longMsg}"),
      (v, _) => ZIO.succeed(v)
    ))

  val partOne: ZIO[Console, Nothing, Unit] =
    layers.map(_.minBy(_.count(Pixel(0)))).map( m => m.count(Pixel(1)) * m.count(Pixel(2))).flatMap {
      answer => console.putStrLn(s"partOne: count of 1s * 2s in 0-est layer is $answer")
    }

  val partTwo: ZIO[Console, Nothing, Unit] =
    layers.map(_.reduceLeft(merge).toString).flatMap(console.putStrLn)

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (partOne *> partTwo).as(0)
}
