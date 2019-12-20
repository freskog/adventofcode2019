package freskog.day15

sealed abstract class Movement extends Product with Serializable {
  def asBigInt: BigInt = this match {
    case North => BigInt(1)
    case South => BigInt(2)
    case West  => BigInt(3)
    case East  => BigInt(4)
  }
  def reverse: Movement = this match {
    case North => South
    case South => North
    case West  => East
    case East  => West
  }
}
case object North extends Movement
case object East  extends Movement
case object South extends Movement
case object West  extends Movement
