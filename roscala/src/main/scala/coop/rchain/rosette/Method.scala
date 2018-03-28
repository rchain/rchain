package coop.rchain.rosette

sealed trait Method extends Ob
case class StdMthd() extends Method {
  override val meta   = null
  override val parent = null
}
