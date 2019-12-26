package freskog.day17

sealed abstract class Move extends Product with Serializable
case object L extends Move
case object R extends Move
case class Forward(step:Int) extends Move

