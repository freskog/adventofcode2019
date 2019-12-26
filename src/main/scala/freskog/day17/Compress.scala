package freskog.day17

object Compress {

  sealed abstract class Token {

    def isSequence: Boolean = this match {
      case Sequence(_, _) => true
      case Lit(_)         => false
    }

    def isLit: Boolean = !isSequence

    def render: String = this match {
      case Lit(R)             => "R"
      case Lit(L)             => "L"
      case Lit(Forward(step)) => s"$step"
      case Sequence(_, l)     => s"${l.map(_.render).mkString(",")}"
    }

  }
  case class Sequence(name: String, l: LazyList[Lit]) extends Token
  case class Lit(move: Move)                          extends Token

  def allSequences(name: String, input: LazyList[Token]): LazyList[Sequence] =
    input
      .dropWhile(_.isSequence)
      .take(10)
      .takeWhile(_.isLit)
      .inits
      .takeWhile(_.nonEmpty)
      .map(
        tokens => Sequence(name, onlyLit(tokens))
      )
      .to(LazyList)

  def onlySeqs(input: LazyList[Token]): LazyList[Sequence] =
    input.collect { case s @ Sequence(_, _) => s }

  def onlyLit(input: LazyList[Token]): LazyList[Lit] =
    input.collect { case l @ Lit(_) => l }

  def replaceLitWithSequence(input: LazyList[Token], seq: Sequence): LazyList[Token] =
    if (input.isEmpty) LazyList()
    else if (input.startsWith(seq.l)) seq #:: replaceLitWithSequence(input.drop(seq.l.length), seq)
    else input.head #:: replaceLitWithSequence(input.tail, seq)

  def isValidSolution(result: LazyList[Token]): Boolean =
    result.forall(_.isSequence) && result.length <= 10

  def solver(input: LazyList[Token]): LazyList[(LazyList[Sequence], Sequence, Sequence, Sequence)] =
    for {
      a      <- allSequences("A", input)
      afterA = replaceLitWithSequence(input, a)
      b      <- allSequences("B", afterA)
      afterB = replaceLitWithSequence(afterA, b)
      c      <- allSequences("C", afterB)
      result = replaceLitWithSequence(afterB, c)
      answer <- LazyList((onlySeqs(result), a, b, c)) if isValidSolution(result)
    } yield answer

  def solve(input: List[Move]): List[String] =
    solver(input.map(Lit).to(LazyList)).head match {
      case (call, a, b, c) =>
        List(call.map(_.name).mkString(","), a.render, b.render, c.render)
    }
}
