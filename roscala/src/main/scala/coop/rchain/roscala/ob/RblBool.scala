package coop.rchain.roscala.ob

case class RblBool(value: Boolean) extends Ob {
  override def toString = s"RblBool($value)"
}
