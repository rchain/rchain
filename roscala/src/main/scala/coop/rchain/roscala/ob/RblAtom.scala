package coop.rchain.roscala.ob


trait RblAtom extends Ob

case class RblBool(value: Boolean) extends RblAtom{
  override def toString = s"RblBool($value)"
}

case class RblString(value: String) extends RblAtom {
  override def toString = s"RblString($value)"
}

case class RblChar(value: Char) extends RblAtom {
  override def toString = s"RblChar($value)"
}
