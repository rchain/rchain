package coop.rchain.rosette

trait Actor extends Ob {
  val extension: Ob
  override val parent: Ob = null
  override val meta: Ob = null
  override val slot: Seq[Ob] = null
}
