package freskog.day9

sealed abstract class Param extends Product with Serializable
case class Addr(position: Position) extends Param
case class Value(v:BigInt) extends Param
case class Rel(v:BigInt) extends Param