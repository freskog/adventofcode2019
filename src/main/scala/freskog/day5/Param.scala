package freskog.day5

sealed abstract class Param extends Product with Serializable
case class Addr(position: Position) extends Param
case class Value(v:Int) extends Param