package coop.rchain.rosette

case class RblTable(map: Map[Ob, Ob]) extends Ob {
  override val meta   = null
  override val parent = null

  //TODO
  val _map = map.updated(StdOprn(null, null, null), GlobalVariable(668))

  def getKey(ob: Ob): Ob = _map.getOrElse(ob, Ob.ABSENT)
}
