package coop.rchain.rosette

case class RblTable(map: Map[Ob, Ob]) extends Ob {
  override val meta   = null
  override val parent = null

  def getKey(ob: Ob): Ob = map.getOrElse(ob, Ob.ABSENT)
}
