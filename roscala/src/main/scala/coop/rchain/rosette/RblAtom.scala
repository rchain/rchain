package coop.rchain.rosette

trait RblAtom extends Ob

case class RblSymbol(value: Symbol) extends RblAtom {
  override val meta   = null
  override val parent = null

  override def toString = s"RblSymbol($value)"
}

case class RblBool(value: Boolean) extends RblAtom {
  override val meta   = null
  override val parent = null

  override def toString = s"RblBool($value)"
}

case class RblString(value: String) extends RblAtom {
  override val meta   = null
  override val parent = null

  override def toString = s"RblString($value)"
}

case class RblChar(value: Char) extends RblAtom {
  override val meta   = null
  override val parent = null

  override def toString = s"RblChar($value)"
}
