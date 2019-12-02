package freskog.day2

sealed abstract class Instruction extends Product with Serializable {
  def size: Int = this match {
    case End          => 1
    case Add(_, _, _) => 4
    case Mul(_, _, _) => 4
  }
}
case class Add(src1: Position, src2: Position, dest: Position) extends Instruction
case class Mul(src1: Position, src2: Position, dest: Position) extends Instruction
case object End                                                extends Instruction
