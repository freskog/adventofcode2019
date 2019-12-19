package freskog.day14

import zio.ZIO

trait Parser {
  val parser:Parser.Service[Any]
}

object Parser {
  trait Service[R] {
    def parseReactions(input: String): ZIO[Any, Nothing, Map[String, Reaction]]
  }

  trait Live extends Parser {
    override val parser: Service[Any] = new Service[Any] {
      import fastparse._, NoWhitespace._
      def chemicalName[_: P]: P[String] = P(CharsWhile(_.isLetter).!)
      def quantity[_: P]: P[Int]        = P(CharsWhile(_.isDigit).!).map(_.toInt)
      def chemical[_: P]: P[Chemical]   = P(quantity ~ " " ~ chemicalName).map { case (q,n) => Chemical(n,q) }

      def reaction[_: P]: P[Reaction] = P(chemical.rep(sep = ", ") ~ " => " ~ chemical).map {
        case (needs, gives) => Reaction(needs.toList, gives)
      }

      def allReactions[_: P]: P[Map[String, Reaction]] =
        reaction.rep(sep = "\n").map(_.map(r => r.provides.name -> r).toMap)

      def parseReactions(input: String): ZIO[Any, Nothing, Map[String, Reaction]] =
        parse(input, allReactions(_)).fold(
          (msg, at, x) => ZIO.dieMessage(s"Can't parse input, ${x.trace(true).longMsg} at $at ($msg)"),
          (map, _) => ZIO.succeed(map)
        )
    }
  }
}
