package freskog.day14

import zio._
import zio.console.Console

object SolutionDay14 extends App {

  def chemist(oreCountRef: Ref[Long],
              poolRef: Ref[Map[String, Int]]): ZIO[Any, Nothing, Chemist.Live with Parser.Live] =
    ZIO.effectTotal(
      new Chemist.Live with Parser.Live {
        override val pool: Ref[Map[String, Int]] = poolRef
        override val oreCount: Ref[Long]         = oreCountRef
      }
    )

  def calculateOreRequired(input: String, n: Int): ZIO[Any, Nothing, Long] =
    for {
      oreGenerated <- Ref.make[Long](0L)
      poolRef      <- Ref.make(Map.empty[String,Int])
      _            <- ZIO.accessM[Chemist](_.chemist.streamFor(input, n)).provideM(chemist(oreGenerated,poolRef))
      n            <- oreGenerated.get
    } yield n

  def oreForFuel(fuels: Int): ZIO[Any, Nothing, Long] =
    freskog
      .decodeAsRawString("freskog/day14/input-day14.txt")
      .orDie
      .flatMap(calculateOreRequired(_, fuels))

  val partOne: ZIO[Console, Nothing, Unit] =
    freskog
      .decodeAsRawString("freskog/day14/input-day14.txt")
      .orDie
      .flatMap(calculateOreRequired(_, 1))
      .flatMap(n => console.putStrLn(s"Need $n ore"))

  def howMuchFuelForGiven1FuelIs(oreCnt: Long): Long = 1L

  val partTwo: ZIO[Console, Nothing, Unit] =
    freskog
      .decodeAsRawString("freskog/day14/input-day14.txt")
      .orDie
      .flatMap(calculateOreRequired(_, 1))
      .map(howMuchFuelForGiven1FuelIs)
      .flatMap(n => console.putStrLn(s"If we have 1 trillion ore, we can generate $n fuel"))

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    partOne.as(0)

}
