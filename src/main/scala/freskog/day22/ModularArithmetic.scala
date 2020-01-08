package freskog.day22

/**
 * based on the tutorial at https://codeforces.com/blog/entry/72593
 */
object ModularArithmetic {

  case class LCF(a: BigInt, b: BigInt, m: BigInt) {
    def andThen(other: LCF): LCF =
      LCF((a * other.a) mod m, ((b * other.a) + other.b) mod m, m)

    def apply(x: BigInt): BigInt =
      ((a * x) + b) mod m

    def composeWithSelf(k: BigInt): LCF =
      LCF(a modPow (k, m), (b * (1 - a.modPow(k, m))) * (1 - a).modInverse(m), m)

    def inverted: LCF =
      LCF(BigInt(1) * a.modInverse(m), (-b) * a.modInverse(m), m)
  }

  def id(m: BigInt): LCF                   = LCF(BigInt(1), BigInt(0), m)
  def dealIntoNewStack(m: BigInt): LCF     = LCF(BigInt(-1), BigInt(-1), m)
  def cut(n: Int, m: BigInt): LCF          = LCF(BigInt(1), BigInt(-n), m)
  def dealWithIncr(n: Int, m: BigInt): LCF = LCF(BigInt(n), BigInt(0), m)

  def shuffle(instructions: String, m: BigInt): LCF = {
    val dealIntoNewStackPat  = "deal into new stack"
    val cutPosPat            = """cut (\d+)""".r
    val cutNegPat            = """cut -(\d+)""".r
    val dealWithIncrementPat = """deal with increment (\d+)""".r

    instructions.split("\n").foldLeft(id(m)) {
      case (acc, `dealIntoNewStackPat`)   => acc andThen dealIntoNewStack(m)
      case (acc, cutPosPat(n))            => acc andThen cut(n.toInt, m)
      case (acc, cutNegPat(n))            => acc andThen cut(n.toInt * -1, m)
      case (acc, dealWithIncrementPat(n)) => acc andThen dealWithIncr(n.toInt, m)
    }
  }

  def part1(instructions: String, m: BigInt): BigInt =
    shuffle(instructions, m)(2019)

  def part2(instructions: String): BigInt =
    shuffle(instructions, BigInt("119315717514047")).composeWithSelf(BigInt("101741582076661")).inverted(2020)
}
