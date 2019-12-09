package freskog.day9

sealed abstract class Param         extends Product with Serializable
sealed abstract class PosParam      extends Param
case class Addr(position: Position) extends PosParam
case class Value(v: BigInt)         extends Param
case class Rel(v: BigInt)           extends PosParam
