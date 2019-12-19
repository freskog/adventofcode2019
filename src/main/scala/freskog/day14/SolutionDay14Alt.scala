package freskog.day14

import zio._
import zio.console.Console
/* Stolen from https://git.njae.me.uk/?p=advent-of-code-19.git;a=blob;f=advent14/src/advent14.hs */
object SolutionDay14Alt extends App {

  case class Reagent(qty: Long, chemical: String)
  case class Reaction(input: Set[Reagent], output: Reagent)

  import fastparse._, NoWhitespace._
  def chemical[_: P]: P[String] = P(CharsWhile(_.isLetter).!)
  def quantity[_: P]: P[Long]    = P(CharsWhile(_.isDigit).!).map(_.toLong)
  def reagent[_: P]: P[Reagent] = P(quantity ~ " " ~ chemical).map { case (q, c) => Reagent(q, c) }

  def reaction[_: P]: P[Reaction] = P(reagent.rep(sep = ", ") ~ " => " ~ reagent).map {
    case (needs, gives) => Reaction(needs.toSet, gives)
  }

  def allReactions[_: P]: P[Map[String, Reaction]] =
    reaction.rep(sep = "\n").map(_.map(r => r.output.chemical -> r).toMap)

  def parseReactions(input: String): ZIO[Any, Nothing, Map[String, Reaction]] =
    parse(input, allReactions(_)).fold(
      (msg, at, x) => ZIO.dieMessage(s"Can't parse input, ${x.trace(true).longMsg} at $at ($msg)"),
      (map, _) => ZIO.succeed(map)
    )

  @scala.annotation.tailrec
  def runReaction(requirements: Map[String, Long], reactions: Map[String, Reaction]): Map[String, Long] =
    if (onlyNeedsOre(requirements)) requirements
    else {
      val outstanding  = nonOreRequirements(requirements)
      val (chem, qty)  = outstanding.minBy { case (_, n) => n }
      val reaction     = reactions(chem)
      val providesQty  = reaction.output.qty
      val applications = math.max(1L, qty / providesQty)
      val newQty       = qty - (applications * providesQty)
      val newRequired  = requirements.updated(chem, newQty)
      runReaction(reaction.input.foldLeft(newRequired)(addRequirement(applications, _, _)), reactions)
    }

  def nonOreRequirements(requirements: Map[String, Long]): Map[String, Long] =
    requirements.collect { case (chem, needs) if chem != "ORE" && needs > 0 => (chem, needs) }

  def onlyNeedsOre(requirements: Map[String, Long]): Boolean =
    requirements.count { case (_, n) => n > 0 } == 1 && requirements.contains("ORE")

  def addRequirement(applications: Long, requirements: Map[String, Long], reagent: Reagent): Map[String, Long] =
    requirements.updated(reagent.chemical, requirements.getOrElse(reagent.chemical, 0L) + (applications * reagent.qty))

  def oreForFuel(reactions:Map[String, Reaction], fuel:Long):Long =
    runReaction(Map("FUEL" -> fuel), reactions)("ORE")

  @scala.annotation.tailrec
  def calculateMaxFuel(reactions:Map[String, Reaction], lowest:Long, highest:Long):Long = {
    if(lowest == highest) lowest
    else {
      val guess = (highest + lowest + 1) / 2
      val oreNeeded = oreForFuel(reactions, guess)
      if(oreNeeded > 1000000000000L) calculateMaxFuel(reactions, lowest, guess - 1)
      else calculateMaxFuel(reactions, guess, highest)
    }
  }

  val partOne: ZIO[Console, Nothing, Unit] =
    freskog
      .decodeAsRawString("freskog/day14/input-day14.txt")
      .orDie
      .flatMap(parseReactions)
      .map(oreForFuel(_, 1L))
      .flatMap(n => zio.console.putStrLn(s"Input requires $n ORE to produce 1 fuel"))

  val partTwo: ZIO[Console, Nothing, Unit] =
    freskog
      .decodeAsRawString("freskog/day14/input-day14.txt")
      .orDie
      .flatMap(parseReactions)
      .map(calculateMaxFuel(_, 0, 1000000000000L))
      .flatMap(n => zio.console.putStrLn(s"Input can generate $n FUEL given 1000000000000 ORE"))


  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (partOne *> partTwo).as(0)
}
