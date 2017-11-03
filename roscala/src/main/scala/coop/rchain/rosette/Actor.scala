package coop.rchain.rosette

import scala.collection.mutable

trait Actor extends Ob {
  val extension: Ob
  override val _slot: mutable.Seq[Ob] = null
}
