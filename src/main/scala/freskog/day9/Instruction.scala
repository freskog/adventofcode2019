package freskog.day9

sealed abstract class Instruction extends Product with Serializable {
  def size: Int = this match {
    case Add(_, _, _)      => 4
    case Mul(_, _, _)      => 4
    case Read(_)           => 2
    case Write(_)          => 2
    case SetRelBase(_)     => 2
    case JumpIfTrue(_, _)  => 3
    case JumpIfFalse(_, _) => 3
    case LessThan(_, _, _) => 4
    case EqualTo(_, _, _)  => 4
    case End               => 1
  }
}

case class Add(src1: Param, src2: Param, dest: PosParam)      extends Instruction
case class Mul(src1: Param, src2: Param, dest: PosParam)      extends Instruction
case class Read(dest: PosParam)                               extends Instruction
case class Write(value: Param)                                extends Instruction
case class SetRelBase(base: Param)                            extends Instruction
case class JumpIfTrue(src: Param, dest: Param)                extends Instruction
case class JumpIfFalse(src: Param, dest: Param)               extends Instruction
case class LessThan(src1: Param, src2: Param, dest: PosParam) extends Instruction
case class EqualTo(src1: Param, src2: Param, dest: PosParam)  extends Instruction
case object End                                               extends Instruction

object Instruction {
  val longest: Int = 4
}
